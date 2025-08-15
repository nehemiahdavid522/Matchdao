# 🤝 MatchDAO - Charity Matching DAO

> **Empowering charitable giving through decentralized community matching** 💝

MatchDAO is a decentralized autonomous organization (DAO) that enables community-driven matching of charitable donations. Fundraisers create campaigns, donors contribute, and DAO members vote on which campaigns deserve matching funds from the community treasury.

## 🌟 Features

- **📋 Campaign Creation**: Anyone can create fundraising campaigns with goals and deadlines
- **💰 Direct Donations**: Support campaigns directly with STX tokens
- **🗳️ Community Voting**: Staked members vote on which campaigns receive matching funds
- **🎯 Matching Funds**: Approved campaigns receive up to 50% matching from the DAO treasury
- **🔒 Stake-Weighted Governance**: Voting power proportional to staked tokens
- **⏰ Time-Bounded Campaigns**: Built-in voting and campaign periods

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- STX tokens for staking and donations

### Installation

```bash
git clone <your-repo>
cd matchdao
clarinet check
```

## 📖 Usage Guide

### 1. 🏦 Stake Tokens to Participate

Before voting, users must stake STX tokens:

```clarity
(contract-call? .Matchdao stake-tokens u1000000) ;; Stake 1 STX
```

### 2. 📝 Create a Campaign

```clarity
(contract-call? .Matchdao create-campaign 
  "Clean Water Project" 
  "Building wells in rural communities" 
  u10000000 ;; Target: 10 STX
  u2880)     ;; Duration: ~2 days
```

### 3. 💝 Donate to Campaigns

```clarity
(contract-call? .Matchdao donate u1 u1000000) ;; Donate 1 STX to campaign #1
```

### 4. 🗳️ Vote on Matching

Staked members vote whether campaigns deserve matching funds:

```clarity
(contract-call? .Matchdao vote-on-matching u1 true) ;; Vote YES for campaign #1
```

### 5. ✅ Finalize Campaign

After voting period ends, finalize to distribute funds:

```clarity
(contract-call? .Matchdao finalize-campaign u1)
```

## 🔍 Read-Only Functions

### Get Campaign Details
```clarity
(contract-call? .Matchdao get-campaign u1)
```

### Check User Stake
```clarity
(contract-call? .Matchdao get-user-stake 'SP1234...)
```

### View DAO Treasury
```clarity
(contract-call? .Matchdao get-dao-treasury)
```

### Check Campaign Status
```clarity
(contract-call? .Matchdao is-campaign-active u1)
```

## ⚙️ Configuration

- **Minimum Stake**: 1 STX (1,000,000 microSTX)
- **Voting Period**: 1,440 blocks (~1 day)
- **Matching Rate**: Up to 50% of raised amount
- **Max Donors per Campaign**: 100
- **Max Voters per Campaign**: 200

## 🛡️ Security Features

- **Emergency Stop**: Contract owner can halt campaigns if needed
- **Stake Requirements**: Only staked members can vote
- **Time Locks**: Built-in voting and campaign periods
- **Single Vote**: Users can only vote once per campaign

## 🏗️ Contract Architecture

The contract uses several key data structures:

- **Campaigns Map**: Stores all campaign details and voting results
- **Donations Map**: Tracks individual donations
- **Votes Map**: Records voting decisions and stakes
- **User Stakes Map**: Manages staked tokens for governance

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch
3. Test with `clarinet test`
4. Submit a pull request

## 📄 License

This project is open source and available under the MIT License.

---


