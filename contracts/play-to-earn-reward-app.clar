(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-balance (err u103))
(define-constant err-invalid-amount (err u104))
(define-constant err-game-paused (err u105))
(define-constant err-task-completed (err u106))
(define-constant err-invalid-task (err u107))
(define-constant err-cooldown-active (err u108))
(define-constant err-achievement-exists (err u109))
(define-constant err-invalid-achievement (err u110))
(define-constant err-invalid-referral (err u111))
(define-constant err-referral-exists (err u112))
(define-constant err-invalid-stake (err u113))
(define-constant err-stake-locked (err u114))
(define-constant err-invalid-pool (err u115))
(define-constant err-lottery-closed (err u116))
(define-constant err-lottery-active (err u117))
(define-constant err-no-winner (err u118))

(define-data-var game-paused bool false)
(define-data-var total-players uint u0)
(define-data-var reward-rate uint u10)
(define-data-var daily-reward uint u100)
(define-data-var task-cooldown uint u86400)
(define-data-var total-achievements uint u0)
(define-data-var referral-rate uint u10)
(define-data-var total-referrals uint u0)
(define-data-var total-staked uint u0)
(define-data-var total-stakes uint u0)
(define-data-var current-lottery-round uint u0)
(define-data-var lottery-ticket-price uint u50)

(define-data-var redemption-active bool false)
(define-data-var redemption-rate uint u1)
(define-data-var total-redemptions uint u0)
(define-data-var total-redemption-stx uint u0)

(define-map players principal {
    points: uint,
    total-earned: uint,
    tasks-completed: uint,
    last-daily-claim: uint,
    registration-block: uint,
    is-active: bool
})

(define-map tasks uint {
    name: (string-ascii 50),
    reward-points: uint,
    is-active: bool,
    completion-limit: uint
})

(define-map player-tasks {player: principal, task-id: uint} {
    completed-count: uint,
    last-completion: uint
})

(define-map leaderboard uint principal)
(define-data-var leaderboard-size uint u0)

(define-map admin-list principal bool)

(define-map achievements uint {
    name: (string-ascii 50),
    description: (string-ascii 100),
    points-requirement: uint,
    tasks-requirement: uint,
    special-condition: uint,
    bonus-points: uint,
    is-active: bool
})

(define-map player-achievements {player: principal, achievement-id: uint} {
    earned: bool,
    earned-at: uint
})

(define-map referral-codes (string-ascii 8) principal)

(define-map player-referrals principal {
    referrer: (optional principal),
    referees-count: uint,
    total-referral-earned: uint,
    referral-code: (string-ascii 8)
})

(define-map staking-pools uint {
    name: (string-ascii 30),
    apr-rate: uint,
    lock-duration: uint,
    min-stake: uint,
    is-active: bool,
    penalty-rate: uint
})

(define-map player-stakes {player: principal, stake-id: uint} {
    pool-id: uint,
    amount: uint,
    start-time: uint,
    claimed-rewards: uint,
    is-active: bool
})

(define-map stake-counters principal uint)

(define-map lottery-rounds uint {
    is-active: bool,
    prize-pool: uint,
    ticket-price: uint,
    end-block: uint,
    total-tickets: uint,
    winner: (optional principal),
    draw-block: uint
})

(define-map lottery-tickets {round-id: uint, ticket-id: uint} principal)

(define-map player-lottery-tickets {player: principal, round-id: uint} uint)

(define-private (is-owner (caller principal))
    (is-eq caller contract-owner))

(define-private (is-admin (caller principal))
    (default-to false (map-get? admin-list caller)))

(define-private (is-game-active)
    (not (var-get game-paused)))

(define-private (get-current-time)
    stacks-block-height)

(define-private (generate-referral-code (seed uint))
    (let ((code-index (mod seed u10)))
        (if (is-eq code-index u0) "ABCD1234"
            (if (is-eq code-index u1) "EFGH5678"
                (if (is-eq code-index u2) "IJKL9012"
                    (if (is-eq code-index u3) "MNOP3456"
                        (if (is-eq code-index u4) "QRST7890"
                            (if (is-eq code-index u5) "UVWX1357"
                                (if (is-eq code-index u6) "YZAB2468"
                                    (if (is-eq code-index u7) "CDEF9753"
                                        (if (is-eq code-index u8) "GHIJ8642"
                                            "KLMN0987")))))))))))

(define-private (calculate-staking-rewards (pool-id uint) (amount uint) (duration-blocks uint))
    (match (map-get? staking-pools pool-id)
        pool-data
            (let ((apr (get apr-rate pool-data)))
                (let ((blocks-per-year u52560))
                    (/ (* amount apr duration-blocks) (* blocks-per-year u100))))
        u0))

(define-private (get-next-stake-id (player principal))
    (+ (default-to u0 (map-get? stake-counters player)) u1))

(define-private (generate-random-winner (round-id uint) (total-tickets uint))
    (let ((round-data (unwrap! (map-get? lottery-rounds round-id) u0)))
        (let ((draw-block (get draw-block round-data)))
            (let ((random-seed (+ draw-block round-id)))
                (mod random-seed total-tickets)))))

(define-private (process-referral-reward (referee principal) (points-earned uint))
    (match (map-get? player-referrals referee)
        referee-data
            (match (get referrer referee-data)
                referrer-principal
                    (let ((referral-reward (/ (* points-earned (var-get referral-rate)) u100)))
                        (if (> referral-reward u0)
                            (begin
                                (let ((referrer-data (unwrap! (map-get? players referrer-principal) false)))
                                    (map-set players referrer-principal {
                                        points: (+ (get points referrer-data) referral-reward),
                                        total-earned: (+ (get total-earned referrer-data) referral-reward),
                                        tasks-completed: (get tasks-completed referrer-data),
                                        last-daily-claim: (get last-daily-claim referrer-data),
                                        registration-block: (get registration-block referrer-data),
                                        is-active: (get is-active referrer-data)
                                    }))
                                (let ((current-referrer-data (default-to {referrer: none, referees-count: u0, total-referral-earned: u0, referral-code: "NONE0000"} 
                                                                         (map-get? player-referrals referrer-principal))))
                                    (map-set player-referrals referrer-principal {
                                        referrer: (get referrer current-referrer-data),
                                        referees-count: (get referees-count current-referrer-data),
                                        total-referral-earned: (+ (get total-referral-earned current-referrer-data) referral-reward),
                                        referral-code: (get referral-code current-referrer-data)
                                    }))
                                true)
                            false))
                false)
        false))

(define-private (check-achievements (player principal))
    (let ((player-data (unwrap! (map-get? players player) false)))
        (begin
            (check-single-achievement player u1 (get total-earned player-data) (get tasks-completed player-data))
            (check-single-achievement player u2 (get total-earned player-data) (get tasks-completed player-data))
            (check-single-achievement player u3 (get total-earned player-data) (get tasks-completed player-data))
            true)))

(define-private (check-single-achievement (player principal) (achievement-id uint) (total-earned uint) (tasks-completed uint))
    (match (map-get? achievements achievement-id)
        achievement-data 
            (let ((already-earned (match (map-get? player-achievements {player: player, achievement-id: achievement-id})
                                         some-achievement (get earned some-achievement)
                                         false)))
                (if (and 
                        (get is-active achievement-data)
                        (>= total-earned (get points-requirement achievement-data))
                        (>= tasks-completed (get tasks-requirement achievement-data))
                        (not already-earned))
                    (begin
                        (map-set player-achievements {player: player, achievement-id: achievement-id} {
                            earned: true,
                            earned-at: (get-current-time)
                        })
                        (let ((player-data (unwrap! (map-get? players player) false)))
                            (map-set players player {
                                points: (+ (get points player-data) (get bonus-points achievement-data)),
                                total-earned: (+ (get total-earned player-data) (get bonus-points achievement-data)),
                                tasks-completed: (get tasks-completed player-data),
                                last-daily-claim: (get last-daily-claim player-data),
                                registration-block: (get registration-block player-data),
                                is-active: (get is-active player-data)
                            }))
                        true)
                    false))
        false))

(define-private (update-leaderboard (player principal))
    (let ((player-data (unwrap! (map-get? players player) false)))
        (let ((current-size (var-get leaderboard-size)))
            (if (< current-size u10)
                (begin
                    (map-set leaderboard current-size player)
                    (var-set leaderboard-size (+ current-size u1))
                    true)
                (let ((lowest-rank (- current-size u1)))
                    (let ((lowest-player (unwrap! (map-get? leaderboard lowest-rank) false)))
                        (let ((lowest-data (unwrap! (map-get? players lowest-player) false)))
                            (if (> (get points player-data) (get points lowest-data))
                                (begin
                                    (map-set leaderboard lowest-rank player)
                                    true)
                                false))))))))

(define-public (register-player)
    (let ((caller tx-sender))
        (asserts! (is-game-active) err-game-paused)
        (asserts! (is-none (map-get? players caller)) err-already-exists)
        (let ((ref-code (generate-referral-code (get-current-time))))
            (map-set players caller {
                points: u0,
                total-earned: u0,
                tasks-completed: u0,
                last-daily-claim: u0,
                registration-block: (get-current-time),
                is-active: true
            })
            (map-set referral-codes ref-code caller)
            (map-set player-referrals caller {
                referrer: none,
                referees-count: u0,
                total-referral-earned: u0,
                referral-code: ref-code
            })
            (var-set total-players (+ (var-get total-players) u1))
            (ok ref-code))))

(define-public (register-with-referral (referral-code (string-ascii 8)))
    (let ((caller tx-sender))
        (asserts! (is-game-active) err-game-paused)
        (asserts! (is-none (map-get? players caller)) err-already-exists)
        (let ((referrer (unwrap! (map-get? referral-codes referral-code) err-invalid-referral)))
            (asserts! (not (is-eq caller referrer)) err-invalid-referral)
            (let ((ref-code (generate-referral-code (get-current-time))))
                (map-set players caller {
                    points: u0,
                    total-earned: u0,
                    tasks-completed: u0,
                    last-daily-claim: u0,
                    registration-block: (get-current-time),
                    is-active: true
                })
                (map-set referral-codes ref-code caller)
                (map-set player-referrals caller {
                    referrer: (some referrer),
                    referees-count: u0,
                    total-referral-earned: u0,
                    referral-code: ref-code
                })
                (let ((referrer-data (default-to {referrer: none, referees-count: u0, total-referral-earned: u0, referral-code: "NONE0000"} 
                                                  (map-get? player-referrals referrer))))
                    (map-set player-referrals referrer {
                        referrer: (get referrer referrer-data),
                        referees-count: (+ (get referees-count referrer-data) u1),
                        total-referral-earned: (get total-referral-earned referrer-data),
                        referral-code: (get referral-code referrer-data)
                    }))
                (var-set total-players (+ (var-get total-players) u1))
                (var-set total-referrals (+ (var-get total-referrals) u1))
                (ok ref-code)))))

(define-public (complete-task (task-id uint))
    (let ((caller tx-sender))
        (asserts! (is-game-active) err-game-paused)
        (let ((player-data (unwrap! (map-get? players caller) err-not-found)))
            (let ((task-data (unwrap! (map-get? tasks task-id) err-invalid-task)))
                (asserts! (get is-active task-data) err-invalid-task)
                (let ((player-task-key {player: caller, task-id: task-id}))
                    (let ((player-task-data (default-to {completed-count: u0, last-completion: u0} 
                                                        (map-get? player-tasks player-task-key))))
                        (asserts! (< (get completed-count player-task-data) (get completion-limit task-data)) 
                                  err-task-completed)
                        (asserts! (> (get-current-time) (+ (get last-completion player-task-data) (var-get task-cooldown))) 
                                  err-cooldown-active)
                        (let ((reward-points (get reward-points task-data)))
                            (map-set player-tasks player-task-key {
                                completed-count: (+ (get completed-count player-task-data) u1),
                                last-completion: (get-current-time)
                            })
                            (map-set players caller {
                                points: (+ (get points player-data) reward-points),
                                total-earned: (+ (get total-earned player-data) reward-points),
                                tasks-completed: (+ (get tasks-completed player-data) u1),
                                last-daily-claim: (get last-daily-claim player-data),
                                registration-block: (get registration-block player-data),
                                is-active: (get is-active player-data)
                            })
                            (update-leaderboard caller)
                            (check-achievements caller)
                            (process-referral-reward caller reward-points)
                            (ok reward-points))))))))

(define-public (claim-daily-reward)
    (let ((caller tx-sender))
        (asserts! (is-game-active) err-game-paused)
        (let ((player-data (unwrap! (map-get? players caller) err-not-found)))
            (asserts! (> (get-current-time) (+ (get last-daily-claim player-data) u144)) err-cooldown-active)
            (let ((daily-points (var-get daily-reward)))
                (map-set players caller {
                    points: (+ (get points player-data) daily-points),
                    total-earned: (+ (get total-earned player-data) daily-points),
                    tasks-completed: (get tasks-completed player-data),
                    last-daily-claim: (get-current-time),
                    registration-block: (get registration-block player-data),
                    is-active: (get is-active player-data)
                })
                (update-leaderboard caller)
                (check-achievements caller)
                (process-referral-reward caller daily-points)
                (ok daily-points)))))

(define-public (spend-points (amount uint))
    (let ((caller tx-sender))
        (asserts! (is-game-active) err-game-paused)
        (asserts! (> amount u0) err-invalid-amount)
        (let ((player-data (unwrap! (map-get? players caller) err-not-found)))
            (asserts! (>= (get points player-data) amount) err-insufficient-balance)
            (map-set players caller {
                points: (- (get points player-data) amount),
                total-earned: (get total-earned player-data),
                tasks-completed: (get tasks-completed player-data),
                last-daily-claim: (get last-daily-claim player-data),
                registration-block: (get registration-block player-data),
                is-active: (get is-active player-data)
            })
            (ok amount))))

(define-public (transfer-points (recipient principal) (amount uint))
    (let ((caller tx-sender))
        (asserts! (is-game-active) err-game-paused)
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (not (is-eq caller recipient)) err-invalid-amount)
        (let ((sender-data (unwrap! (map-get? players caller) err-not-found)))
            (let ((recipient-data (unwrap! (map-get? players recipient) err-not-found)))
                (asserts! (>= (get points sender-data) amount) err-insufficient-balance)
                (map-set players caller {
                    points: (- (get points sender-data) amount),
                    total-earned: (get total-earned sender-data),
                    tasks-completed: (get tasks-completed sender-data),
                    last-daily-claim: (get last-daily-claim sender-data),
                    registration-block: (get registration-block sender-data),
                    is-active: (get is-active sender-data)
                })
                (map-set players recipient {
                    points: (+ (get points recipient-data) amount),
                    total-earned: (get total-earned recipient-data),
                    tasks-completed: (get tasks-completed recipient-data),
                    last-daily-claim: (get last-daily-claim recipient-data),
                    registration-block: (get registration-block recipient-data),
                    is-active: (get is-active recipient-data)
                })
                (update-leaderboard recipient)
                (ok amount)))))


(define-public (set-redemption-rate (new-rate uint))
    (let ((caller tx-sender))
        (asserts! (is-owner caller) err-owner-only)
        (asserts! (> new-rate u0) err-invalid-amount)
        (var-set redemption-rate new-rate)
        (ok new-rate)))

(define-public (toggle-redemption)
    (let ((caller tx-sender))
        (asserts! (is-owner caller) err-owner-only)
        (let ((active (var-get redemption-active)))
            (var-set redemption-active (not active))
            (ok (not active)))))

(define-public (redeem (points uint))
    (let ((caller tx-sender))
        (asserts! (is-game-active) err-game-paused)
        (asserts! (> points u0) err-invalid-amount)
        (asserts! (var-get redemption-active) err-invalid-amount)
        (let ((player-data (unwrap! (map-get? players caller) err-not-found)))
            (asserts! (>= (get points player-data) points) err-insufficient-balance)
            (let ((rate (var-get redemption-rate)))
                (let ((amount (* points rate)))
                    (match (as-contract (stx-transfer? amount tx-sender caller))
                        transfer-ok
                            (begin
                                (map-set players caller {
                                    points: (- (get points player-data) points),
                                    total-earned: (get total-earned player-data),
                                    tasks-completed: (get tasks-completed player-data),
                                    last-daily-claim: (get last-daily-claim player-data),
                                    registration-block: (get registration-block player-data),
                                    is-active: (get is-active player-data)
                                })
                                (var-set total-redemptions (+ (var-get total-redemptions) u1))
                                (var-set total-redemption-stx (+ (var-get total-redemption-stx) amount))
                                (ok amount))
                        err-code
                            (err err-code)))))))

(define-read-only (get-redemption-config)
    {
        active: (var-get redemption-active),
        rate: (var-get redemption-rate),
        total-redemptions: (var-get total-redemptions),
        total-redemption-stx: (var-get total-redemption-stx)
    })

(define-public (create-task (task-id uint) (name (string-ascii 50)) (reward-points uint) (completion-limit uint))
    (let ((caller tx-sender))
        (asserts! (or (is-owner caller) (is-admin caller)) err-owner-only)
        (asserts! (is-none (map-get? tasks task-id)) err-already-exists)
        (asserts! (> reward-points u0) err-invalid-amount)
        (map-set tasks task-id {
            name: name,
            reward-points: reward-points,
            is-active: true,
            completion-limit: completion-limit
        })
        (ok true)))

(define-public (toggle-task (task-id uint))
    (let ((caller tx-sender))
        (asserts! (or (is-owner caller) (is-admin caller)) err-owner-only)
        (let ((task-data (unwrap! (map-get? tasks task-id) err-invalid-task)))
            (map-set tasks task-id {
                name: (get name task-data),
                reward-points: (get reward-points task-data),
                is-active: (not (get is-active task-data)),
                completion-limit: (get completion-limit task-data)
            })
            (ok (not (get is-active task-data))))))

(define-public (pause-game)
    (let ((caller tx-sender))
        (asserts! (is-owner caller) err-owner-only)
        (var-set game-paused true)
        (ok true)))

(define-public (resume-game)
    (let ((caller tx-sender))
        (asserts! (is-owner caller) err-owner-only)
        (var-set game-paused false)
        (ok true)))

(define-public (set-reward-rate (new-rate uint))
    (let ((caller tx-sender))
        (asserts! (is-owner caller) err-owner-only)
        (var-set reward-rate new-rate)
        (ok new-rate)))

(define-public (set-daily-reward (new-reward uint))
    (let ((caller tx-sender))
        (asserts! (is-owner caller) err-owner-only)
        (var-set daily-reward new-reward)
        (ok new-reward)))

(define-public (set-referral-rate (new-rate uint))
    (let ((caller tx-sender))
        (asserts! (is-owner caller) err-owner-only)
        (asserts! (<= new-rate u100) err-invalid-amount)
        (var-set referral-rate new-rate)
        (ok new-rate)))

(define-public (add-admin (admin principal))
    (let ((caller tx-sender))
        (asserts! (is-owner caller) err-owner-only)
        (map-set admin-list admin true)
        (ok true)))

(define-public (remove-admin (admin principal))
    (let ((caller tx-sender))
        (asserts! (is-owner caller) err-owner-only)
        (map-delete admin-list admin)
        (ok true)))

(define-public (create-achievement (achievement-id uint) (name (string-ascii 50)) (description (string-ascii 100)) 
                                  (points-requirement uint) (tasks-requirement uint) (special-condition uint) (bonus-points uint))
    (let ((caller tx-sender))
        (asserts! (or (is-owner caller) (is-admin caller)) err-owner-only)
        (asserts! (is-none (map-get? achievements achievement-id)) err-achievement-exists)
        (map-set achievements achievement-id {
            name: name,
            description: description,
            points-requirement: points-requirement,
            tasks-requirement: tasks-requirement,
            special-condition: special-condition,
            bonus-points: bonus-points,
            is-active: true
        })
        (var-set total-achievements (+ (var-get total-achievements) u1))
        (ok true)))

(define-public (toggle-achievement (achievement-id uint))
    (let ((caller tx-sender))
        (asserts! (or (is-owner caller) (is-admin caller)) err-owner-only)
        (let ((achievement-data (unwrap! (map-get? achievements achievement-id) err-invalid-achievement)))
            (map-set achievements achievement-id {
                name: (get name achievement-data),
                description: (get description achievement-data),
                points-requirement: (get points-requirement achievement-data),
                tasks-requirement: (get tasks-requirement achievement-data),
                special-condition: (get special-condition achievement-data),
                bonus-points: (get bonus-points achievement-data),
                is-active: (not (get is-active achievement-data))
            })
            (ok (not (get is-active achievement-data))))))

(define-public (create-staking-pool (pool-id uint) (name (string-ascii 30)) (apr-rate uint) (lock-duration uint) (min-stake uint) (penalty-rate uint))
    (let ((caller tx-sender))
        (asserts! (or (is-owner caller) (is-admin caller)) err-owner-only)
        (asserts! (is-none (map-get? staking-pools pool-id)) err-already-exists)
        (asserts! (<= apr-rate u200) err-invalid-amount)
        (asserts! (<= penalty-rate u50) err-invalid-amount)
        (map-set staking-pools pool-id {
            name: name,
            apr-rate: apr-rate,
            lock-duration: lock-duration,
            min-stake: min-stake,
            is-active: true,
            penalty-rate: penalty-rate
        })
        (ok true)))

(define-public (toggle-staking-pool (pool-id uint))
    (let ((caller tx-sender))
        (asserts! (or (is-owner caller) (is-admin caller)) err-owner-only)
        (let ((pool-data (unwrap! (map-get? staking-pools pool-id) err-invalid-pool)))
            (map-set staking-pools pool-id {
                name: (get name pool-data),
                apr-rate: (get apr-rate pool-data),
                lock-duration: (get lock-duration pool-data),
                min-stake: (get min-stake pool-data),
                is-active: (not (get is-active pool-data)),
                penalty-rate: (get penalty-rate pool-data)
            })
            (ok (not (get is-active pool-data))))))

(define-public (stake-points (pool-id uint) (amount uint))
    (let ((caller tx-sender))
        (asserts! (is-game-active) err-game-paused)
        (asserts! (> amount u0) err-invalid-amount)
        (let ((player-data (unwrap! (map-get? players caller) err-not-found)))
            (let ((pool-data (unwrap! (map-get? staking-pools pool-id) err-invalid-pool)))
                (asserts! (get is-active pool-data) err-invalid-pool)
                (asserts! (>= amount (get min-stake pool-data)) err-invalid-amount)
                (asserts! (>= (get points player-data) amount) err-insufficient-balance)
                (let ((stake-id (get-next-stake-id caller)))
                    (map-set players caller {
                        points: (- (get points player-data) amount),
                        total-earned: (get total-earned player-data),
                        tasks-completed: (get tasks-completed player-data),
                        last-daily-claim: (get last-daily-claim player-data),
                        registration-block: (get registration-block player-data),
                        is-active: (get is-active player-data)
                    })
                    (map-set player-stakes {player: caller, stake-id: stake-id} {
                        pool-id: pool-id,
                        amount: amount,
                        start-time: (get-current-time),
                        claimed-rewards: u0,
                        is-active: true
                    })
                    (map-set stake-counters caller stake-id)
                    (var-set total-staked (+ (var-get total-staked) amount))
                    (var-set total-stakes (+ (var-get total-stakes) u1))
                    (ok stake-id))))))

(define-public (claim-staking-rewards (stake-id uint))
    (let ((caller tx-sender))
        (asserts! (is-game-active) err-game-paused)
        (let ((stake-key {player: caller, stake-id: stake-id}))
            (let ((stake-data (unwrap! (map-get? player-stakes stake-key) err-invalid-stake)))
                (asserts! (get is-active stake-data) err-invalid-stake)
                (let ((pool-data (unwrap! (map-get? staking-pools (get pool-id stake-data)) err-invalid-pool)))
                    (let ((duration (- (get-current-time) (get start-time stake-data))))
                        (let ((total-rewards (calculate-staking-rewards (get pool-id stake-data) (get amount stake-data) duration)))
                            (let ((claimable-rewards (- total-rewards (get claimed-rewards stake-data))))
                                (if (> claimable-rewards u0)
                                    (begin
                                        (let ((player-data (unwrap! (map-get? players caller) err-not-found)))
                                            (map-set players caller {
                                                points: (+ (get points player-data) claimable-rewards),
                                                total-earned: (+ (get total-earned player-data) claimable-rewards),
                                                tasks-completed: (get tasks-completed player-data),
                                                last-daily-claim: (get last-daily-claim player-data),
                                                registration-block: (get registration-block player-data),
                                                is-active: (get is-active player-data)
                                            }))
                                        (map-set player-stakes stake-key {
                                            pool-id: (get pool-id stake-data),
                                            amount: (get amount stake-data),
                                            start-time: (get start-time stake-data),
                                            claimed-rewards: total-rewards,
                                            is-active: (get is-active stake-data)
                                        })
                                        (ok claimable-rewards))
                                    (ok u0))))))))))

(define-public (unstake-points (stake-id uint))
    (let ((caller tx-sender))
        (asserts! (is-game-active) err-game-paused)
        (let ((stake-key {player: caller, stake-id: stake-id}))
            (let ((stake-data (unwrap! (map-get? player-stakes stake-key) err-invalid-stake)))
                (asserts! (get is-active stake-data) err-invalid-stake)
                (let ((pool-data (unwrap! (map-get? staking-pools (get pool-id stake-data)) err-invalid-pool)))
                    (let ((duration (- (get-current-time) (get start-time stake-data))))
                        (let ((lock-duration (get lock-duration pool-data)))
                            (let ((penalty-rate (get penalty-rate pool-data)))
                                (let ((is-early-withdraw (< duration lock-duration)))
                                    (let ((penalty (if is-early-withdraw (/ (* (get amount stake-data) penalty-rate) u100) u0)))
                                        (let ((return-amount (- (get amount stake-data) penalty)))
                                            (let ((total-rewards (calculate-staking-rewards (get pool-id stake-data) (get amount stake-data) duration)))
                                                (let ((unclaimed-rewards (- total-rewards (get claimed-rewards stake-data))))
                                                    (let ((final-return (+ return-amount unclaimed-rewards)))
                                                        (let ((player-data (unwrap! (map-get? players caller) err-not-found)))
                                                            (map-set players caller {
                                                                points: (+ (get points player-data) final-return),
                                                                total-earned: (+ (get total-earned player-data) unclaimed-rewards),
                                                                tasks-completed: (get tasks-completed player-data),
                                                                last-daily-claim: (get last-daily-claim player-data),
                                                                registration-block: (get registration-block player-data),
                                                                is-active: (get is-active player-data)
                                                            }))
                                                        (map-set player-stakes stake-key {
                                                            pool-id: (get pool-id stake-data),
                                                            amount: (get amount stake-data),
                                                            start-time: (get start-time stake-data),
                                                            claimed-rewards: (get claimed-rewards stake-data),
                                                            is-active: false
                                                        })
                                                        (var-set total-staked (- (var-get total-staked) (get amount stake-data)))
                                                        (ok final-return)))))))))))))))

(define-public (create-lottery-round (duration-blocks uint) (ticket-price uint))
    (let ((caller tx-sender))
        (asserts! (or (is-owner caller) (is-admin caller)) err-owner-only)
        (let ((current-round (var-get current-lottery-round)))
            (match (map-get? lottery-rounds current-round)
                existing-round (asserts! (not (get is-active existing-round)) err-lottery-active)
                true)
            (let ((new-round-id (+ current-round u1)))
                (map-set lottery-rounds new-round-id {
                    is-active: true,
                    prize-pool: u0,
                    ticket-price: ticket-price,
                    end-block: (+ (get-current-time) duration-blocks),
                    total-tickets: u0,
                    winner: none,
                    draw-block: u0
                })
                (var-set current-lottery-round new-round-id)
                (var-set lottery-ticket-price ticket-price)
                (ok new-round-id)))))

(define-public (buy-lottery-ticket (round-id uint) (num-tickets uint))
    (let ((caller tx-sender))
        (asserts! (is-game-active) err-game-paused)
        (asserts! (> num-tickets u0) err-invalid-amount)
        (let ((round-data (unwrap! (map-get? lottery-rounds round-id) err-not-found)))
            (asserts! (get is-active round-data) err-lottery-closed)
            (asserts! (<= (get-current-time) (get end-block round-data)) err-lottery-closed)
            (let ((total-cost (* (get ticket-price round-data) num-tickets)))
                (let ((player-data (unwrap! (map-get? players caller) err-not-found)))
                    (asserts! (>= (get points player-data) total-cost) err-insufficient-balance)
                    (map-set players caller {
                        points: (- (get points player-data) total-cost),
                        total-earned: (get total-earned player-data),
                        tasks-completed: (get tasks-completed player-data),
                        last-daily-claim: (get last-daily-claim player-data),
                        registration-block: (get registration-block player-data),
                        is-active: (get is-active player-data)
                    })
                    (let ((current-total (get total-tickets round-data)))
                        (let ((new-total (+ current-total num-tickets)))
                            (let ((player-ticket-count (default-to u0 (map-get? player-lottery-tickets {player: caller, round-id: round-id}))))
                                (map-set player-lottery-tickets {player: caller, round-id: round-id} (+ player-ticket-count num-tickets))
                                (map-set lottery-rounds round-id {
                                    is-active: (get is-active round-data),
                                    prize-pool: (+ (get prize-pool round-data) total-cost),
                                    ticket-price: (get ticket-price round-data),
                                    end-block: (get end-block round-data),
                                    total-tickets: new-total,
                                    winner: (get winner round-data),
                                    draw-block: (get draw-block round-data)
                                })
                                (let ((start-ticket current-total))
                                    (fold assign-ticket-to-player 
                                        (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9) 
                                        {player: caller, round: round-id, start: start-ticket, count: num-tickets})
                                    (ok new-total))))))))))

(define-private (assign-ticket-to-player (index uint) (context {player: principal, round: uint, start: uint, count: uint}))
    (if (< index (get count context))
        (begin
            (map-set lottery-tickets 
                {round-id: (get round context), ticket-id: (+ (get start context) index)} 
                (get player context))
            context)
        context))

(define-public (draw-lottery-winner (round-id uint))
    (let ((caller tx-sender))
        (asserts! (or (is-owner caller) (is-admin caller)) err-owner-only)
        (let ((round-data (unwrap! (map-get? lottery-rounds round-id) err-not-found)))
            (asserts! (get is-active round-data) err-lottery-closed)
            (asserts! (> (get-current-time) (get end-block round-data)) err-lottery-active)
            (asserts! (> (get total-tickets round-data) u0) err-no-winner)
            (let ((draw-block (get-current-time)))
                (map-set lottery-rounds round-id {
                    is-active: (get is-active round-data),
                    prize-pool: (get prize-pool round-data),
                    ticket-price: (get ticket-price round-data),
                    end-block: (get end-block round-data),
                    total-tickets: (get total-tickets round-data),
                    winner: (get winner round-data),
                    draw-block: draw-block
                })
                (ok draw-block)))))

(define-public (finalize-lottery-winner (round-id uint))
    (let ((caller tx-sender))
        (asserts! (or (is-owner caller) (is-admin caller)) err-owner-only)
        (let ((round-data (unwrap! (map-get? lottery-rounds round-id) err-not-found)))
            (asserts! (get is-active round-data) err-lottery-closed)
            (asserts! (> (get draw-block round-data) u0) err-no-winner)
            (asserts! (is-none (get winner round-data)) err-already-exists)
            (let ((winning-ticket-id (generate-random-winner round-id (get total-tickets round-data))))
                (let ((winner-principal (unwrap! (map-get? lottery-tickets {round-id: round-id, ticket-id: winning-ticket-id}) err-no-winner)))
                    (let ((prize-amount (get prize-pool round-data)))
                        (let ((winner-data (unwrap! (map-get? players winner-principal) err-not-found)))
                            (map-set players winner-principal {
                                points: (+ (get points winner-data) prize-amount),
                                total-earned: (+ (get total-earned winner-data) prize-amount),
                                tasks-completed: (get tasks-completed winner-data),
                                last-daily-claim: (get last-daily-claim winner-data),
                                registration-block: (get registration-block winner-data),
                                is-active: (get is-active winner-data)
                            })
                            (map-set lottery-rounds round-id {
                                is-active: false,
                                prize-pool: (get prize-pool round-data),
                                ticket-price: (get ticket-price round-data),
                                end-block: (get end-block round-data),
                                total-tickets: (get total-tickets round-data),
                                winner: (some winner-principal),
                                draw-block: (get draw-block round-data)
                            })
                            (ok winner-principal))))))))

(define-public (award-achievement (player principal) (achievement-id uint))
    (let ((caller tx-sender))
        (asserts! (or (is-owner caller) (is-admin caller)) err-owner-only)
        (asserts! (is-some (map-get? players player)) err-not-found)
        (let ((achievement-data (unwrap! (map-get? achievements achievement-id) err-invalid-achievement)))
            (asserts! (get is-active achievement-data) err-invalid-achievement)
            (let ((already-earned (match (map-get? player-achievements {player: player, achievement-id: achievement-id})
                                         some-achievement (get earned some-achievement)
                                         false)))
                (asserts! (not already-earned) err-achievement-exists))
            (map-set player-achievements {player: player, achievement-id: achievement-id} {
                earned: true,
                earned-at: (get-current-time)
            })
            (let ((player-data (unwrap! (map-get? players player) err-not-found)))
                (map-set players player {
                    points: (+ (get points player-data) (get bonus-points achievement-data)),
                    total-earned: (+ (get total-earned player-data) (get bonus-points achievement-data)),
                    tasks-completed: (get tasks-completed player-data),
                    last-daily-claim: (get last-daily-claim player-data),
                    registration-block: (get registration-block player-data),
                    is-active: (get is-active player-data)
                }))
            (ok true))))

(define-read-only (get-player-data (player principal))
    (map-get? players player))

(define-read-only (get-task-data (task-id uint))
    (map-get? tasks task-id))

(define-read-only (get-player-task-data (player principal) (task-id uint))
    (map-get? player-tasks {player: player, task-id: task-id}))

(define-read-only (get-leaderboard-entry (rank uint))
    (map-get? leaderboard rank))

(define-read-only (get-achievement-data (achievement-id uint))
    (map-get? achievements achievement-id))

(define-read-only (get-player-achievement (player principal) (achievement-id uint))
    (map-get? player-achievements {player: player, achievement-id: achievement-id}))

(define-read-only (get-player-achievements-list (player principal))
    (list 
        (map-get? player-achievements {player: player, achievement-id: u1})
        (map-get? player-achievements {player: player, achievement-id: u2})
        (map-get? player-achievements {player: player, achievement-id: u3})
    ))

(define-read-only (get-referral-data (player principal))
    (map-get? player-referrals player))

(define-read-only (get-referrer-by-code (referral-code (string-ascii 8)))
    (map-get? referral-codes referral-code))

(define-read-only (get-staking-pool (pool-id uint))
    (map-get? staking-pools pool-id))

(define-read-only (get-player-stake (player principal) (stake-id uint))
    (map-get? player-stakes {player: player, stake-id: stake-id}))

(define-read-only (get-stake-rewards (player principal) (stake-id uint))
    (match (map-get? player-stakes {player: player, stake-id: stake-id})
        stake-data
            (if (get is-active stake-data)
                (let ((duration (- (get-current-time) (get start-time stake-data))))
                    (let ((total-rewards (calculate-staking-rewards (get pool-id stake-data) (get amount stake-data) duration)))
                        (some (- total-rewards (get claimed-rewards stake-data)))))
                none)
        none))

(define-read-only (get-lottery-round (round-id uint))
    (map-get? lottery-rounds round-id))

(define-read-only (get-player-lottery-tickets (player principal) (round-id uint))
    (default-to u0 (map-get? player-lottery-tickets {player: player, round-id: round-id})))

(define-read-only (get-current-lottery)
    (let ((current-round (var-get current-lottery-round)))
        (map-get? lottery-rounds current-round)))

(define-read-only (get-game-stats)
    {
        total-players: (var-get total-players),
        game-paused: (var-get game-paused),
        reward-rate: (var-get reward-rate),
        daily-reward: (var-get daily-reward),
        leaderboard-size: (var-get leaderboard-size),
        total-achievements: (var-get total-achievements),
        referral-rate: (var-get referral-rate),
        total-referrals: (var-get total-referrals),
        total-staked: (var-get total-staked),
        total-stakes: (var-get total-stakes),
        current-lottery-round: (var-get current-lottery-round)
    })

(define-read-only (is-player-registered (player principal))
    (is-some (map-get? players player)))

(define-read-only (can-claim-daily (player principal))
    (match (map-get? players player)
        player-data (> (get-current-time) (+ (get last-daily-claim player-data) u144))
        false))
