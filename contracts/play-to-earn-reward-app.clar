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

(define-data-var game-paused bool false)
(define-data-var total-players uint u0)
(define-data-var reward-rate uint u10)
(define-data-var daily-reward uint u100)
(define-data-var task-cooldown uint u86400)
(define-data-var total-achievements uint u0)

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

(define-private (is-owner (caller principal))
    (is-eq caller contract-owner)
)

(define-private (is-admin (caller principal))
    (default-to false (map-get? admin-list caller))
)

(define-private (is-game-active)
    (not (var-get game-paused))
)

(define-private (get-current-time)
    stacks-block-height
)

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
        (map-set players caller {
            points: u0,
            total-earned: u0,
            tasks-completed: u0,
            last-daily-claim: u0,
            registration-block: (get-current-time),
            is-active: true
        })
        (var-set total-players (+ (var-get total-players) u1))
        (ok true)))

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

(define-read-only (get-game-stats)
    {
        total-players: (var-get total-players),
        game-paused: (var-get game-paused),
        reward-rate: (var-get reward-rate),
        daily-reward: (var-get daily-reward),
        leaderboard-size: (var-get leaderboard-size),
        total-achievements: (var-get total-achievements)
    })

(define-read-only (is-player-registered (player principal))
    (is-some (map-get? players player)))

(define-read-only (can-claim-daily (player principal))
    (match (map-get? players player)
        player-data (> (get-current-time) (+ (get last-daily-claim player-data) u144))
        false))
