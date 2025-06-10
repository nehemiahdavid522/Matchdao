;; title: Matchdao
;; version: 1.0.0
;; summary: Charity Matching DAO - Fundraisers matched by on-chain voters
;; description: A decentralized autonomous organization for matching charitable donations through community voting

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_CAMPAIGN_NOT_FOUND (err u101))
(define-constant ERR_CAMPAIGN_ENDED (err u102))
(define-constant ERR_CAMPAIGN_ACTIVE (err u103))
(define-constant ERR_INSUFFICIENT_FUNDS (err u104))
(define-constant ERR_ALREADY_VOTED (err u105))
(define-constant ERR_INVALID_AMOUNT (err u106))
(define-constant ERR_VOTING_ENDED (err u107))
(define-constant ERR_VOTING_ACTIVE (err u108))
(define-constant ERR_MINIMUM_STAKE_REQUIRED (err u109))

(define-data-var next-campaign-id uint u1)
(define-data-var dao-treasury uint u0)
(define-data-var minimum-stake uint u1000000)
(define-data-var voting-period uint u1440)

(define-map campaigns
  uint
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    target-amount: uint,
    raised-amount: uint,
    matching-pool: uint,
    end-block: uint,
    voting-end-block: uint,
    active: bool,
    votes-for: uint,
    votes-against: uint
  }
)

(define-map donations
  { campaign-id: uint, donor: principal }
  { amount: uint, block-height: uint }
)

(define-map votes
  { campaign-id: uint, voter: principal }
  { vote: bool, stake: uint, block-height: uint }
)

(define-map user-stakes
  principal
  uint
)

(define-map campaign-donors
  uint
  (list 100 principal)
)

(define-map campaign-voters
  uint
  (list 200 principal)
)

(define-public (stake-tokens (amount uint))
  (begin
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set user-stakes tx-sender 
      (+ (default-to u0 (map-get? user-stakes tx-sender)) amount))
    (var-set dao-treasury (+ (var-get dao-treasury) amount))
    (ok true)
  )
)

(define-public (unstake-tokens (amount uint))
  (let ((current-stake (default-to u0 (map-get? user-stakes tx-sender))))
    (asserts! (>= current-stake amount) ERR_INSUFFICIENT_FUNDS)
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    (map-set user-stakes tx-sender (- current-stake amount))
    (var-set dao-treasury (- (var-get dao-treasury) amount))
    (ok true)
  )
)

(define-public (create-campaign (title (string-ascii 100)) (description (string-ascii 500)) (target-amount uint) (duration uint))
  (let ((campaign-id (var-get next-campaign-id))
        (end-block (+ stacks-block-height duration))
        (voting-end (+ stacks-block-height (var-get voting-period))))
    (asserts! (> target-amount u0) ERR_INVALID_AMOUNT)
    (map-set campaigns campaign-id {
      creator: tx-sender,
      title: title,
      description: description,
      target-amount: target-amount,
      raised-amount: u0,
      matching-pool: u0,
      end-block: end-block,
      voting-end-block: voting-end,
      active: true,
      votes-for: u0,
      votes-against: u0
    })
    (var-set next-campaign-id (+ campaign-id u1))
    (ok campaign-id)
  )
)

(define-public (donate (campaign-id uint) (amount uint))
  (let ((campaign (unwrap! (map-get? campaigns campaign-id) ERR_CAMPAIGN_NOT_FOUND)))
    (asserts! (get active campaign) ERR_CAMPAIGN_ENDED)
    (asserts! (< stacks-block-height (get end-block campaign)) ERR_CAMPAIGN_ENDED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set donations { campaign-id: campaign-id, donor: tx-sender }
      { amount: amount, block-height: stacks-block-height })
    
    (map-set campaigns campaign-id
      (merge campaign { raised-amount: (+ (get raised-amount campaign) amount) }))
    
    (map-set campaign-donors campaign-id
      (unwrap! (as-max-len? (append (default-to (list) (map-get? campaign-donors campaign-id)) tx-sender) u100) ERR_INVALID_AMOUNT))
    
    (ok true)
  )
)

(define-public (vote-on-matching (campaign-id uint) (support bool))
  (let ((campaign (unwrap! (map-get? campaigns campaign-id) ERR_CAMPAIGN_NOT_FOUND))
        (user-stake (default-to u0 (map-get? user-stakes tx-sender))))
    
    (asserts! (>= user-stake (var-get minimum-stake)) ERR_MINIMUM_STAKE_REQUIRED)
    (asserts! (< stacks-block-height (get voting-end-block campaign)) ERR_VOTING_ENDED)
    (asserts! (is-none (map-get? votes { campaign-id: campaign-id, voter: tx-sender })) ERR_ALREADY_VOTED)
    
    (map-set votes { campaign-id: campaign-id, voter: tx-sender }
      { vote: support, stake: user-stake, block-height: stacks-block-height })
    
    (map-set campaign-voters campaign-id
      (unwrap! (as-max-len? (append (default-to (list) (map-get? campaign-voters campaign-id)) tx-sender) u200) ERR_INVALID_AMOUNT))
    
    (if support
      (map-set campaigns campaign-id
        (merge campaign { votes-for: (+ (get votes-for campaign) user-stake) }))
      (map-set campaigns campaign-id
        (merge campaign { votes-against: (+ (get votes-against campaign) user-stake) })))
    
    (ok true)
  )
)

(define-public (finalize-campaign (campaign-id uint))
  (let ((campaign (unwrap! (map-get? campaigns campaign-id) ERR_CAMPAIGN_NOT_FOUND)))
    (asserts! (get active campaign) ERR_CAMPAIGN_ENDED)
    (asserts! (>= stacks-block-height (get voting-end-block campaign)) ERR_VOTING_ACTIVE)
    
    (let ((matching-approved (> (get votes-for campaign) (get votes-against campaign)))
          (raised-amount (get raised-amount campaign))
          (matching-amount (if matching-approved (/ raised-amount u2) u0)))
      
      (map-set campaigns campaign-id
        (merge campaign { 
          active: false, 
          matching-pool: matching-amount 
        }))
      
      (if matching-approved
        (begin
          (try! (as-contract (stx-transfer? matching-amount tx-sender (get creator campaign))))
          (var-set dao-treasury (- (var-get dao-treasury) matching-amount)))
        true)
      
      (try! (as-contract (stx-transfer? raised-amount tx-sender (get creator campaign))))
      (ok matching-approved)
    )
  )
)

(define-public (emergency-stop (campaign-id uint))
  (let ((campaign (unwrap! (map-get? campaigns campaign-id) ERR_CAMPAIGN_NOT_FOUND)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (get active campaign) ERR_CAMPAIGN_ENDED)
    
    (map-set campaigns campaign-id (merge campaign { active: false }))
    (ok true)
  )
)

(define-public (update-voting-period (new-period uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set voting-period new-period)
    (ok true)
  )
)

(define-public (update-minimum-stake (new-stake uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set minimum-stake new-stake)
    (ok true)
  )
)

(define-read-only (get-campaign (campaign-id uint))
  (map-get? campaigns campaign-id)
)

(define-read-only (get-donation (campaign-id uint) (donor principal))
  (map-get? donations { campaign-id: campaign-id, donor: donor })
)

(define-read-only (get-vote (campaign-id uint) (voter principal))
  (map-get? votes { campaign-id: campaign-id, voter: voter })
)

(define-read-only (get-user-stake (user principal))
  (default-to u0 (map-get? user-stakes user))
)

(define-read-only (get-dao-treasury)
  (var-get dao-treasury)
)

(define-read-only (get-next-campaign-id)
  (var-get next-campaign-id)
)

(define-read-only (get-voting-period)
  (var-get voting-period)
)

(define-read-only (get-minimum-stake)
  (var-get minimum-stake)
)

(define-read-only (get-campaign-donors (campaign-id uint))
  (default-to (list) (map-get? campaign-donors campaign-id))
)

(define-read-only (get-campaign-voters (campaign-id uint))
  (default-to (list) (map-get? campaign-voters campaign-id))
)

(define-read-only (is-campaign-active (campaign-id uint))
  (match (map-get? campaigns campaign-id)
    campaign (and (get active campaign) (< stacks-block-height (get end-block campaign)))
    false
  )
)

(define-read-only (get-matching-ratio (campaign-id uint))
  (match (map-get? campaigns campaign-id)
    campaign 
      (if (> (get raised-amount campaign) u0)
        (/ (* (get matching-pool campaign) u100) (get raised-amount campaign))
        u0)
    u0
  )
)