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
(define-constant ERR_INVALID_TIER (err u110))
(define-constant ERR_INSUFFICIENT_REPUTATION (err u111))
(define-constant ERR_REWARD_ALREADY_CLAIMED (err u112))
(define-constant ERR_NO_REWARDS_AVAILABLE (err u113))

(define-data-var next-campaign-id uint u1)
(define-data-var dao-treasury uint u0)
(define-data-var minimum-stake uint u1000000)
(define-data-var voting-period uint u1440)
(define-data-var reward-pool uint u0)
(define-data-var total-reputation-points uint u0)

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

(define-map user-reputation
  principal
  {
    total-points: uint,
    donations-made: uint,
    votes-cast: uint,
    campaigns-created: uint,
    successful-campaigns: uint,
    tier: uint,
    last-activity: uint
  }
)

(define-map user-rewards
  principal
  {
    total-earned: uint,
    total-claimed: uint,
    bronze-rewards: uint,
    silver-rewards: uint,
    gold-rewards: uint,
    platinum-rewards: uint,
    last-claim-block: uint
  }
)

(define-map tier-requirements
  uint
  {
    min-points: uint,
    reward-multiplier: uint,
    bonus-percentage: uint,
    name: (string-ascii 20)
  }
)

(define-map monthly-leaderboard
  uint
  (list 10 principal)
)

(define-map user-achievements
  principal
  {
    first-donation: bool,
    first-vote: bool,
    first-campaign: bool,
    mega-donor: bool,
    community-champion: bool,
    campaign-master: bool,
    loyalty-badge: bool
  }
)

(define-private (initialize-tier-system)
  (begin
    (map-set tier-requirements u0 { min-points: u0, reward-multiplier: u1, bonus-percentage: u0, name: "Bronze" })
    (map-set tier-requirements u1 { min-points: u1000, reward-multiplier: u2, bonus-percentage: u5, name: "Silver" })
    (map-set tier-requirements u2 { min-points: u5000, reward-multiplier: u3, bonus-percentage: u10, name: "Gold" })
    (map-set tier-requirements u3 { min-points: u15000, reward-multiplier: u5, bonus-percentage: u20, name: "Platinum" })
    true
  )
)

(define-private (calculate-user-tier (total-points uint))
  (if (>= total-points u15000)
    u3
    (if (>= total-points u5000)
      u2
      (if (>= total-points u1000)
        u1
        u0
      )
    )
  )
)

(define-private (award-reputation-points (user principal) (points uint) (activity-type (string-ascii 20)))
  (let ((current-rep (default-to { total-points: u0, donations-made: u0, votes-cast: u0, campaigns-created: u0, successful-campaigns: u0, tier: u0, last-activity: u0 } 
                                 (map-get? user-reputation user)))
        (new-total-points (+ (get total-points current-rep) points))
        (new-tier (calculate-user-tier new-total-points)))
    
    (map-set user-reputation user
      (merge current-rep {
        total-points: new-total-points,
        tier: new-tier,
        last-activity: stacks-block-height
      }))
    
    (var-set total-reputation-points (+ (var-get total-reputation-points) points))
    (update-user-rewards user points)
    (check-and-award-achievements user activity-type)
    true
  )
)

(define-private (update-user-rewards (user principal) (points uint))
  (let ((current-rewards (default-to { total-earned: u0, total-claimed: u0, bronze-rewards: u0, silver-rewards: u0, gold-rewards: u0, platinum-rewards: u0, last-claim-block: u0 } 
                                     (map-get? user-rewards user)))
        (user-tier (get tier (default-to { total-points: u0, donations-made: u0, votes-cast: u0, campaigns-created: u0, successful-campaigns: u0, tier: u0, last-activity: u0 } 
                                         (map-get? user-reputation user))))
        (reward-amount (/ (* points (get reward-multiplier (default-to { min-points: u0, reward-multiplier: u1, bonus-percentage: u0, name: "Bronze" } 
                                                                       (map-get? tier-requirements user-tier)))) u1)))
    
    (map-set user-rewards user
      (merge current-rewards {
        total-earned: (+ (get total-earned current-rewards) reward-amount)
      }))
    
    (var-set reward-pool (+ (var-get reward-pool) reward-amount))
    true
  )
)

(define-private (check-and-award-achievements (user principal) (activity-type (string-ascii 20)))
  (let ((current-achievements (default-to { first-donation: false, first-vote: false, first-campaign: false, mega-donor: false, community-champion: false, campaign-master: false, loyalty-badge: false } 
                                          (map-get? user-achievements user)))
        (user-rep (default-to { total-points: u0, donations-made: u0, votes-cast: u0, campaigns-created: u0, successful-campaigns: u0, tier: u0, last-activity: u0 } 
                              (map-get? user-reputation user))))
    
    (map-set user-achievements user
      (merge current-achievements {
        first-donation: (or (get first-donation current-achievements) (is-eq activity-type "donation")),
        first-vote: (or (get first-vote current-achievements) (is-eq activity-type "vote")),
        first-campaign: (or (get first-campaign current-achievements) (is-eq activity-type "campaign")),
        mega-donor: (or (get mega-donor current-achievements) (>= (get donations-made user-rep) u10)),
        community-champion: (or (get community-champion current-achievements) (>= (get votes-cast user-rep) u50)),
        campaign-master: (or (get campaign-master current-achievements) (>= (get successful-campaigns user-rep) u5)),
        loyalty-badge: (or (get loyalty-badge current-achievements) (>= (get total-points user-rep) u10000))
      }))
    true
  )
)

(define-public (stake-tokens (amount uint))
  (begin
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set user-stakes tx-sender 
      (+ (default-to u0 (map-get? user-stakes tx-sender)) amount))
    (var-set dao-treasury (+ (var-get dao-treasury) amount))
    (award-reputation-points tx-sender (/ amount u100000) "stake")
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
        (voting-end (+ stacks-block-height (var-get voting-period)))
        (current-rep (default-to { total-points: u0, donations-made: u0, votes-cast: u0, campaigns-created: u0, successful-campaigns: u0, tier: u0, last-activity: u0 } 
                                 (map-get? user-reputation tx-sender))))
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
    
    (map-set user-reputation tx-sender
      (merge current-rep {
        campaigns-created: (+ (get campaigns-created current-rep) u1)
      }))
    
    (award-reputation-points tx-sender u500 "campaign")
    (ok campaign-id)
  )
)

(define-public (donate (campaign-id uint) (amount uint))
  (let ((campaign (unwrap! (map-get? campaigns campaign-id) ERR_CAMPAIGN_NOT_FOUND))
        (current-rep (default-to { total-points: u0, donations-made: u0, votes-cast: u0, campaigns-created: u0, successful-campaigns: u0, tier: u0, last-activity: u0 } 
                                 (map-get? user-reputation tx-sender)))
        (user-tier (get tier current-rep))
        (tier-bonus (get bonus-percentage (default-to { min-points: u0, reward-multiplier: u1, bonus-percentage: u0, name: "Bronze" } 
                                                      (map-get? tier-requirements user-tier))))
        (bonus-amount (/ (* amount tier-bonus) u100)))
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
    
    (map-set user-reputation tx-sender
      (merge current-rep {
        donations-made: (+ (get donations-made current-rep) u1)
      }))
    
    (award-reputation-points tx-sender (+ (/ amount u10000) bonus-amount) "donation")
    (ok true)
  )
)

(define-public (vote-on-matching (campaign-id uint) (support bool))
  (let ((campaign (unwrap! (map-get? campaigns campaign-id) ERR_CAMPAIGN_NOT_FOUND))
        (user-stake (default-to u0 (map-get? user-stakes tx-sender)))
        (current-rep (default-to { total-points: u0, donations-made: u0, votes-cast: u0, campaigns-created: u0, successful-campaigns: u0, tier: u0, last-activity: u0 } 
                                 (map-get? user-reputation tx-sender))))
    
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
    
    (map-set user-reputation tx-sender
      (merge current-rep {
        votes-cast: (+ (get votes-cast current-rep) u1)
      }))
    
    (award-reputation-points tx-sender u200 "vote")
    (ok true)
  )
)

(define-public (finalize-campaign (campaign-id uint))
  (let ((campaign (unwrap! (map-get? campaigns campaign-id) ERR_CAMPAIGN_NOT_FOUND))
        (creator (get creator campaign))
        (current-rep (default-to { total-points: u0, donations-made: u0, votes-cast: u0, campaigns-created: u0, successful-campaigns: u0, tier: u0, last-activity: u0 } 
                                 (map-get? user-reputation creator))))
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
          (try! (as-contract (stx-transfer? matching-amount tx-sender creator)))
          (var-set dao-treasury (- (var-get dao-treasury) matching-amount))
          (map-set user-reputation creator
            (merge current-rep {
              successful-campaigns: (+ (get successful-campaigns current-rep) u1)
            }))
          (award-reputation-points creator u1000 "success"))
        true)
      
      (try! (as-contract (stx-transfer? raised-amount tx-sender creator)))
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

(define-public (claim-tier-rewards)
  (let ((user-rewards-data (default-to { total-earned: u0, total-claimed: u0, bronze-rewards: u0, silver-rewards: u0, gold-rewards: u0, platinum-rewards: u0, last-claim-block: u0 } 
                                       (map-get? user-rewards tx-sender)))
        (unclaimed-amount (- (get total-earned user-rewards-data) (get total-claimed user-rewards-data))))
    (asserts! (> unclaimed-amount u0) ERR_NO_REWARDS_AVAILABLE)
    (asserts! (>= (var-get reward-pool) unclaimed-amount) ERR_INSUFFICIENT_FUNDS)
    
    (try! (as-contract (stx-transfer? unclaimed-amount tx-sender tx-sender)))
    
    (map-set user-rewards tx-sender
      (merge user-rewards-data {
        total-claimed: (get total-earned user-rewards-data),
        last-claim-block: stacks-block-height
      }))
    
    (var-set reward-pool (- (var-get reward-pool) unclaimed-amount))
    (ok unclaimed-amount)
  )
)

(define-public (boost-campaign-with-reputation (campaign-id uint))
  (let ((campaign (unwrap! (map-get? campaigns campaign-id) ERR_CAMPAIGN_NOT_FOUND))
        (user-rep (default-to { total-points: u0, donations-made: u0, votes-cast: u0, campaigns-created: u0, successful-campaigns: u0, tier: u0, last-activity: u0 } 
                              (map-get? user-reputation tx-sender)))
        (reputation-points (get total-points user-rep))
        (boost-amount (/ reputation-points u100)))
    
    (asserts! (get active campaign) ERR_CAMPAIGN_ENDED)
    (asserts! (>= reputation-points u1000) ERR_INSUFFICIENT_REPUTATION)
    (asserts! (>= (var-get dao-treasury) boost-amount) ERR_INSUFFICIENT_FUNDS)
    
    (map-set campaigns campaign-id
      (merge campaign { 
        raised-amount: (+ (get raised-amount campaign) boost-amount)
      }))
    
    (var-set dao-treasury (- (var-get dao-treasury) boost-amount))
    
    (map-set user-reputation tx-sender
      (merge user-rep {
        total-points: (- reputation-points u500)
      }))
    
    (ok boost-amount)
  )
)

(define-public (create-reputation-milestone (milestone-points uint) (reward-amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (> milestone-points u0) ERR_INVALID_AMOUNT)
    (asserts! (> reward-amount u0) ERR_INVALID_AMOUNT)
    
    (var-set reward-pool (+ (var-get reward-pool) reward-amount))
    (ok true)
  )
)

(define-public (update-monthly-leaderboard (month uint))
  (let ((current-month-leaders (default-to (list) (map-get? monthly-leaderboard month))))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
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

(define-read-only (get-user-reputation (user principal))
  (default-to { total-points: u0, donations-made: u0, votes-cast: u0, campaigns-created: u0, successful-campaigns: u0, tier: u0, last-activity: u0 } 
              (map-get? user-reputation user))
)

(define-read-only (get-user-rewards (user principal))
  (default-to { total-earned: u0, total-claimed: u0, bronze-rewards: u0, silver-rewards: u0, gold-rewards: u0, platinum-rewards: u0, last-claim-block: u0 } 
              (map-get? user-rewards user))
)

(define-read-only (get-tier-requirements (tier uint))
  (default-to { min-points: u0, reward-multiplier: u1, bonus-percentage: u0, name: "Bronze" } 
              (map-get? tier-requirements tier))
)

(define-read-only (get-user-achievements (user principal))
  (default-to { first-donation: false, first-vote: false, first-campaign: false, mega-donor: false, community-champion: false, campaign-master: false, loyalty-badge: false } 
              (map-get? user-achievements user))
)

(define-read-only (get-monthly-leaderboard (month uint))
  (default-to (list) (map-get? monthly-leaderboard month))
)

(define-read-only (get-reward-pool)
  (var-get reward-pool)
)

(define-read-only (get-total-reputation-points)
  (var-get total-reputation-points)
)

(define-read-only (get-user-tier-info (user principal))
  (let ((user-rep (get-user-reputation user))
        (tier (get tier user-rep)))
    {
      user-tier: tier,
      tier-name: (get name (get-tier-requirements tier)),
      total-points: (get total-points user-rep),
      next-tier-points: (if (< tier u3) 
                          (get min-points (get-tier-requirements (+ tier u1)))
                          u0),
      rewards-earned: (get total-earned (get-user-rewards user)),
      rewards-claimed: (get total-claimed (get-user-rewards user))
    }
  )
)

(define-read-only (calculate-reputation-boost (user principal) (base-amount uint))
  (let ((user-tier (get tier (get-user-reputation user)))
        (bonus-percentage (get bonus-percentage (get-tier-requirements user-tier))))
    (+ base-amount (/ (* base-amount bonus-percentage) u100))
  )
)