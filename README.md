# 🎮 Play-to-Earn Reward App

A blockchain-based gaming platform where players earn rewards by completing tasks and engaging with the ecosystem. Built on Stacks using Clarity smart contracts.

## ✨ Features

- 🏆 **Player Registration**: Join the game and start earning
- 📋 **Task System**: Complete various tasks to earn points
- 💰 **Daily Rewards**: Claim bonus points every day
- 🔄 **Point Transfers**: Send points to other players
- 🛍️ **Point Spending**: Use points for in-game purchases
- 🥇 **Leaderboard**: Compete with other players
- 👑 **Admin Controls**: Manage tasks and game settings

## 🚀 Quick Start

### Prerequisites
- Clarinet CLI installed
- Stacks wallet for testing

### Installation
```bash
git clone <your-repo>
cd play-to-earn-reward-app
clarinet check
```

## 🎯 Core Functions

### Player Functions

#### Register as Player
```clarity
(contract-call? .play-to-earn-reward-app register-player)
```

#### Complete a Task
```clarity
(contract-call? .play-to-earn-reward-app complete-task u1)
```

#### Claim Daily Reward
```clarity
(contract-call? .play-to-earn-reward-app claim-daily-reward)
```

#### Transfer Points
```clarity
(contract-call? .play-to-earn-reward-app transfer-points 'SP1234... u50)
```

#### Spend Points
```clarity
(contract-call? .play-to-earn-reward-app spend-points u100)
```

### Admin Functions

#### Create Task
```clarity
(contract-call? .play-to-earn-reward-app create-task u1 "Complete Tutorial" u50 u1)
```

#### Pause/Resume Game
```clarity
(contract-call? .play-to-earn-reward-app pause-game)
(contract-call? .play-to-earn-reward-app resume-game)
```

#### Set Rewards
```clarity
(contract-call? .play-to-earn-reward-app set-daily-reward u150)
```

## 📊 Read-Only Functions

### Get Player Data
```clarity
(contract-call? .play-to-earn-reward-app get-player-data 'SP1234...)
```

### Check Game Stats
```clarity
(contract-call? .play-to-earn-reward-app get-game-stats)
```

### View Leaderboard
```clarity
(contract-call? .play-to-earn-reward-app get-leaderboard-entry u0)
```

## 🏗️ Contract Structure

### Data Maps
- **players**: Store player information (points, tasks completed, etc.)
- **tasks**: Define available tasks and rewards
- **player-tasks**: Track individual task completions
- **leaderboard**: Top 10 players ranking
- **admin-list**: Authorized administrators

### Key Variables
- `game-paused`: Control game state
- `total-players`: Track registered players
- `reward-rate`: Point multiplier
- `daily-reward`: Daily bonus amount
- `task-cooldown`: Time between task completions

## 🎮 Game Mechanics

### Points System
- Complete tasks to earn points
- Claim daily rewards (100 points default)
- Points can be transferred between players
- Spend points for rewards

### Task System
- Tasks have completion limits
- Cooldown period between completions (24 hours default)
- Configurable rewards per task

### Leaderboard
- Top 10 players by total points
- Updates automatically when points change
- Real-time ranking system

## 🔐 Security Features

- Owner-only administrative functions
- Multi-admin support with role management
- Game pause mechanism for emergencies
- Input validation and error handling
- Cooldown mechanisms to prevent spam

## 🧪 Testing

Run the test suite:
```bash
clarinet test
```

Check contract syntax:
```bash
clarinet check
```

## 📜 Error Codes

- `u100`: Owner only operation
- `u101`: Player/resource not found  
- `u102`: Already exists
- `u103`: Insufficient balance
- `u104`: Invalid amount
- `u105`: Game paused
- `u106`: Task already completed
- `u107`: Invalid task
- `u108`: Cooldown still active

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test thoroughly
5. Submit a pull request

## 📄 License

MIT License - see LICENSE file for details

---

Built with ❤️ on Stacks blockchain
