;; Bounty Board - Decentralized Task Marketplace
;; Post bounties for tasks with escrow payments and reputation-based verification

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-input (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-bounty-taken (err u105))
(define-constant err-bounty-completed (err u106))
(define-constant err-not-hunter (err u107))
(define-constant err-already-reviewed (err u108))
(define-constant err-contract-paused (err u109))

;; Data Variables
(define-data-var next-bounty-id uint u1)
(define-data-var platform-fee uint u300) ;; 3% in basis points
(define-data-var min-bounty-amount uint u5000000) ;; 5 STX minimum
(define-data-var contract-paused bool false)
(define-data-var reviewer-reward uint u500000) ;; 0.5 STX per review

;; Data Maps
(define-map bounties
    { bounty-id: uint }
    {
        poster: principal,
        hunter: (optional principal),
        title: (string-ascii 64),
        description: (string-ascii 512),
        category: (string-ascii 32),
        reward-amount: uint,
        required-reviews: uint,
        current-reviews: uint,
        approved-reviews: uint,
        deadline: uint,
        status: (string-ascii 16),
        difficulty: (string-ascii 16),
        submission-url: (string-ascii 256),
        submission-hash: (string-ascii 64),
        created-at: uint,
        claimed-at: uint,
        completed-at: uint
    }
)

(define-map bounty-reviews
    { bounty-id: uint, reviewer: principal }
    {
        is-approved: bool,
        rating: uint,
        review-comment: (string-ascii 256),
        review-date: uint
    }
)

(define-map user-profiles
    { user: principal }
    {
        username: (string-ascii 32),
        bounties-posted: uint,
        bounties-completed: uint,
        total-earned: uint,
        total-spent: uint,
        reputation-score: uint,
        success-rate: uint,
        reviews-given: uint,
        join-date: uint
    }
)

(define-map category-stats
    { category: (string-ascii 32) }
    {
        total-bounties: uint,
        completed-bounties: uint,
        total-rewards: uint,
        avg-completion-time: uint,
        top-hunter: (optional principal)
    }
)

(define-map verified-reviewers
    { reviewer: principal }
    {
        is-verified: bool,
        reviews-completed: uint,
        accuracy-score: uint,
        specialization: (string-ascii 64)
    }
)

(define-map bounty-disputes
    { bounty-id: uint }
    {
        disputed-by: principal,
        dispute-reason: (string-ascii 256),
        dispute-date: uint,
        is-resolved: bool,
        resolution: (string-ascii 256)
    }
)

;; Authorization Functions
(define-private (is-owner)
    (is-eq tx-sender contract-owner)
)

(define-private (is-bounty-poster (bounty-id uint))
    (match (map-get? bounties { bounty-id: bounty-id })
        bounty (is-eq tx-sender (get poster bounty))
        false
    )
)

(define-private (is-bounty-hunter (bounty-id uint))
    (match (map-get? bounties { bounty-id: bounty-id })
        bounty (is-eq (some tx-sender) (get hunter bounty))
        false
    )
)

(define-private (is-verified-reviewer)
    (match (map-get? verified-reviewers { reviewer: tx-sender })
        reviewer-data (get is-verified reviewer-data)
        false
    )
)

;; Admin Functions
(define-public (set-platform-fee (new-fee uint))
    (begin
        (asserts! (is-owner) err-owner-only)
        (asserts! (<= new-fee u1000) err-invalid-input)
        (var-set platform-fee new-fee)
        (ok true)
    )
)

(define-public (set-reviewer-reward (new-reward uint))
    (begin
        (asserts! (is-owner) err-owner-only)
        (var-set reviewer-reward new-reward)
        (ok true)
    )
)

(define-public (verify-reviewer (reviewer principal) (specialization (string-ascii 64)))
    (begin
        (asserts! (is-owner) err-owner-only)
        (map-set verified-reviewers
            { reviewer: reviewer }
            {
                is-verified: true,
                reviews-completed: u0,
                accuracy-score: u100,
                specialization: specialization
            }
        )
        (ok true)
    )
)

(define-public (pause-contract)
    (begin
        (asserts! (is-owner) err-owner-only)
        (var-set contract-paused true)
        (ok true)
    )
)

(define-public (unpause-contract)
    (begin
        (asserts! (is-owner) err-owner-only)
        (var-set contract-paused false)
        (ok true)
    )
)

;; User Profile Functions
(define-public (create-profile (username (string-ascii 32)))
    (let
        ((existing-profile (map-get? user-profiles { user: tx-sender })))
        (begin
            (asserts! (not (var-get contract-paused)) err-contract-paused)
            (asserts! (is-none existing-profile) err-bounty-completed)
            (asserts! (> (len username) u0) err-invalid-input)
            
            (map-set user-profiles
                { user: tx-sender }
                {
                    username: username,
                    bounties-posted: u0,
                    bounties-completed: u0,
                    total-earned: u0,
                    total-spent: u0,
                    reputation-score: u100,
                    success-rate: u0,
                    reviews-given: u0,
                    join-date: u0
                }
            )
            (ok true)
        )
    )
)

;; Bounty Creation and Management
(define-public (post-bounty
    (title (string-ascii 64))
    (description (string-ascii 512))
    (category (string-ascii 32))
    (reward-amount uint)
    (required-reviews uint)
    (deadline uint)
    (difficulty (string-ascii 16)))
    (let
        (
            (bounty-id (var-get next-bounty-id))
            (platform-fee-amount (/ (* reward-amount (var-get platform-fee)) u10000))
            (total-cost (+ reward-amount platform-fee-amount))
        )
        (begin
            (asserts! (not (var-get contract-paused)) err-contract-paused)
            (asserts! (> (len title) u0) err-invalid-input)
            (asserts! (>= reward-amount (var-get min-bounty-amount)) err-insufficient-funds)
            (asserts! (and (>= required-reviews u1) (<= required-reviews u5)) err-invalid-input)
            (asserts! (> deadline u0) err-invalid-input)
            
            ;; Escrow total amount
            (try! (stx-transfer? total-cost tx-sender (as-contract tx-sender)))
            
            ;; Create bounty
            (map-set bounties
                { bounty-id: bounty-id }
                {
                    poster: tx-sender,
                    hunter: none,
                    title: title,
                    description: description,
                    category: category,
                    reward-amount: reward-amount,
                    required-reviews: required-reviews,
                    current-reviews: u0,
                    approved-reviews: u0,
                    deadline: deadline,
                    status: "open",
                    difficulty: difficulty,
                    submission-url: "",
                    submission-hash: "",
                    created-at: u0,
                    claimed-at: u0,
                    completed-at: u0
                }
            )
            
            ;; Update poster stats
            (update-poster-stats tx-sender reward-amount)
            
            ;; Update category stats
            (update-category-stats category reward-amount true)
            
            ;; Increment bounty ID
            (var-set next-bounty-id (+ bounty-id u1))
            
            (ok bounty-id)
        )
    )
)

(define-public (claim-bounty (bounty-id uint))
    (let
        (
            (bounty-data (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
        )
        (begin
            (asserts! (not (var-get contract-paused)) err-contract-paused)
            (asserts! (is-eq (get status bounty-data) "open") err-bounty-taken)
            (asserts! (not (is-eq tx-sender (get poster bounty-data))) err-unauthorized)
            (asserts! (>= (get deadline bounty-data) u0) err-invalid-input)
            
            ;; Update bounty status
            (map-set bounties
                { bounty-id: bounty-id }
                (merge bounty-data {
                    hunter: (some tx-sender),
                    status: "claimed",
                    claimed-at: u0
                })
            )
            
            (ok true)
        )
    )
)

(define-public (submit-work
    (bounty-id uint)
    (submission-url (string-ascii 256))
    (submission-hash (string-ascii 64)))
    (let
        (
            (bounty-data (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
        )
        (begin
            (asserts! (not (var-get contract-paused)) err-contract-paused)
            (asserts! (is-bounty-hunter bounty-id) err-not-hunter)
            (asserts! (is-eq (get status bounty-data) "claimed") err-invalid-input)
            (asserts! (> (len submission-url) u0) err-invalid-input)
            (asserts! (> (len submission-hash) u0) err-invalid-input)
            
            ;; Update bounty with submission
            (map-set bounties
                { bounty-id: bounty-id }
                (merge bounty-data {
                    status: "submitted",
                    submission-url: submission-url,
                    submission-hash: submission-hash,
                    completed-at: u0
                })
            )
            
            (ok true)
        )
    )
)

(define-public (review-submission
    (bounty-id uint)
    (is-approved bool)
    (rating uint)
    (review-comment (string-ascii 256)))
    (let
        (
            (bounty-data (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
            (existing-review (map-get? bounty-reviews { bounty-id: bounty-id, reviewer: tx-sender }))
        )
        (begin
            (asserts! (not (var-get contract-paused)) err-contract-paused)
            (asserts! (is-verified-reviewer) err-unauthorized)
            (asserts! (is-eq (get status bounty-data) "submitted") err-invalid-input)
            (asserts! (is-none existing-review) err-already-reviewed)
            (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-input)
            (asserts! (not (is-eq tx-sender (get poster bounty-data))) err-unauthorized)
            (asserts! (not (is-eq (some tx-sender) (get hunter bounty-data))) err-unauthorized)
            
            ;; Record review
            (map-set bounty-reviews
                { bounty-id: bounty-id, reviewer: tx-sender }
                {
                    is-approved: is-approved,
                    rating: rating,
                    review-comment: review-comment,
                    review-date: u0
                }
            )
            
            ;; Update bounty review counts
            (let
                (
                    (new-review-count (+ (get current-reviews bounty-data) u1))
                    (new-approved-count (if is-approved 
                        (+ (get approved-reviews bounty-data) u1)
                        (get approved-reviews bounty-data)
                    ))
                )
                (begin
                    (map-set bounties
                        { bounty-id: bounty-id }
                        (merge bounty-data {
                            current-reviews: new-review-count,
                            approved-reviews: new-approved-count
                        })
                    )
                    
                    ;; Pay reviewer
                    (try! (as-contract (stx-transfer? (var-get reviewer-reward) tx-sender tx-sender)))
                    
                    ;; Update reviewer stats
                    (update-reviewer-stats tx-sender)
                    
                    ;; Check if enough reviews to finalize
                    (if (>= new-review-count (get required-reviews bounty-data))
                        (finalize-bounty bounty-id)
                        (ok true)
                    )
                )
            )
        )
    )
)

(define-private (finalize-bounty (bounty-id uint))
    (let
        (
            (bounty-data (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
            (hunter (unwrap! (get hunter bounty-data) err-not-hunter))
            (approval-threshold (/ (get required-reviews bounty-data) u2))
        )
        (begin
            ;; Check if majority approved
            (if (> (get approved-reviews bounty-data) approval-threshold)
                (begin
                    ;; Pay hunter
                    (try! (as-contract (stx-transfer? (get reward-amount bounty-data) tx-sender hunter)))
                    
                    ;; Pay platform fee to owner
                    (let
                        ((fee-amount (/ (* (get reward-amount bounty-data) (var-get platform-fee)) u10000)))
                        (try! (as-contract (stx-transfer? fee-amount tx-sender contract-owner)))
                    )
                    
                    ;; Update bounty status
                    (map-set bounties
                        { bounty-id: bounty-id }
                        (merge bounty-data { status: "approved" })
                    )
                    
                    ;; Update hunter stats
                    (update-hunter-stats hunter (get reward-amount bounty-data))
                    
                    ;; Update category stats
                    (update-category-stats (get category bounty-data) u0 false)
                    
                    (ok true)
                )
                (begin
                    ;; Refund poster
                    (try! (as-contract (stx-transfer? (get reward-amount bounty-data) tx-sender (get poster bounty-data))))
                    
                    ;; Update bounty status
                    (map-set bounties
                        { bounty-id: bounty-id }
                        (merge bounty-data { status: "rejected" })
                    )
                    
                    (ok true)
                )
            )
        )
    )
)

(define-public (cancel-bounty (bounty-id uint))
    (let
        (
            (bounty-data (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
        )
        (begin
            (asserts! (not (var-get contract-paused)) err-contract-paused)
            (asserts! (is-bounty-poster bounty-id) err-unauthorized)
            (asserts! (is-eq (get status bounty-data) "open") err-bounty-taken)
            
            ;; Refund poster (reward + platform fee)
            (let
                (
                    (platform-fee-amount (/ (* (get reward-amount bounty-data) (var-get platform-fee)) u10000))
                    (refund-amount (+ (get reward-amount bounty-data) platform-fee-amount))
                )
                (begin
                    (try! (as-contract (stx-transfer? refund-amount tx-sender (get poster bounty-data))))
                    
                    ;; Update bounty status
                    (map-set bounties
                        { bounty-id: bounty-id }
                        (merge bounty-data { status: "cancelled" })
                    )
                    
                    (ok true)
                )
            )
        )
    )
)

;; Dispute Management
(define-public (raise-dispute
    (bounty-id uint)
    (dispute-reason (string-ascii 256)))
    (let
        (
            (bounty-data (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
        )
        (begin
            (asserts! (not (var-get contract-paused)) err-contract-paused)
            (asserts! (or (is-bounty-poster bounty-id) (is-bounty-hunter bounty-id)) err-unauthorized)
            (asserts! (> (len dispute-reason) u0) err-invalid-input)
            
            (map-set bounty-disputes
                { bounty-id: bounty-id }
                {
                    disputed-by: tx-sender,
                    dispute-reason: dispute-reason,
                    dispute-date: u0,
                    is-resolved: false,
                    resolution: ""
                }
            )
            
            ;; Update bounty status
            (map-set bounties
                { bounty-id: bounty-id }
                (merge bounty-data { status: "disputed" })
            )
            
            (ok true)
        )
    )
)

(define-public (resolve-dispute
    (bounty-id uint)
    (resolution (string-ascii 256))
    (refund-poster bool))
    (let
        (
            (bounty-data (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
            (dispute-data (unwrap! (map-get? bounty-disputes { bounty-id: bounty-id }) err-not-found))
        )
        (begin
            (asserts! (is-owner) err-owner-only)
            (asserts! (not (get is-resolved dispute-data)) err-bounty-completed)
            
            ;; Update dispute
            (map-set bounty-disputes
                { bounty-id: bounty-id }
                (merge dispute-data {
                    is-resolved: true,
                    resolution: resolution
                })
            )
            
            ;; Handle refund based on resolution
            (if refund-poster
                (begin
                    (try! (as-contract (stx-transfer? (get reward-amount bounty-data) tx-sender (get poster bounty-data))))
                    (map-set bounties
                        { bounty-id: bounty-id }
                        (merge bounty-data { status: "refunded" })
                    )
                )
                (let
                    ((hunter (unwrap! (get hunter bounty-data) err-not-hunter)))
                    (begin
                        (try! (as-contract (stx-transfer? (get reward-amount bounty-data) tx-sender hunter)))
                        (map-set bounties
                            { bounty-id: bounty-id }
                            (merge bounty-data { status: "resolved" })
                        )
                        (update-hunter-stats hunter (get reward-amount bounty-data))
                    )
                )
            )
            
            (ok true)
        )
    )
)

;; Helper Functions (Optimized)
(define-private (update-poster-stats (poster principal) (amount uint))
    (match (map-get? user-profiles { user: poster })
        profile (begin
            (map-set user-profiles
                { user: poster }
                (merge profile {
                    bounties-posted: (+ (get bounties-posted profile) u1),
                    total-spent: (+ (get total-spent profile) amount)
                })
            )
            true
        )
        true
    )
)

(define-private (update-hunter-stats (hunter principal) (amount uint))
    (match (map-get? user-profiles { user: hunter })
        profile (let
            (
                (new-completed (+ (get bounties-completed profile) u1))
                (new-earned (+ (get total-earned profile) amount))
                (new-success-rate (if (> (get bounties-posted profile) u0)
                    (/ (* new-completed u100) (get bounties-posted profile))
                    u100
                ))
            )
            (begin
                (map-set user-profiles
                    { user: hunter }
                    (merge profile {
                        bounties-completed: new-completed,
                        total-earned: new-earned,
                        success-rate: new-success-rate,
                        reputation-score: (+ (get reputation-score profile) u10)
                    })
                )
                true
            )
        )
        true
    )
)

(define-private (update-reviewer-stats (reviewer principal))
    (match (map-get? verified-reviewers { reviewer: reviewer })
        reviewer-data (begin
            (map-set verified-reviewers
                { reviewer: reviewer }
                (merge reviewer-data {
                    reviews-completed: (+ (get reviews-completed reviewer-data) u1)
                })
            )
            true
        )
        true
    )
)

(define-private (update-category-stats (category (string-ascii 32)) (reward uint) (is-new bool))
    (let
        (
            (stats (default-to 
                { total-bounties: u0, completed-bounties: u0, total-rewards: u0, avg-completion-time: u0, top-hunter: none }
                (map-get? category-stats { category: category })
            ))
        )
        (begin
            (map-set category-stats
                { category: category }
                {
                    total-bounties: (if is-new (+ (get total-bounties stats) u1) (get total-bounties stats)),
                    completed-bounties: (if is-new (get completed-bounties stats) (+ (get completed-bounties stats) u1)),
                    total-rewards: (+ (get total-rewards stats) reward),
                    avg-completion-time: (get avg-completion-time stats),
                    top-hunter: (get top-hunter stats)
                }
            )
            true
        )
    )
)

;; Optimized batch query functions
(define-public (get-multiple-bounties (bounty-ids (list 20 uint)))
    (ok (map get-bounty-safe bounty-ids))
)

(define-private (get-bounty-safe (bounty-id uint))
    (map-get? bounties { bounty-id: bounty-id })
)

;; Read-Only Functions
(define-read-only (get-bounty (bounty-id uint))
    (map-get? bounties { bounty-id: bounty-id })
)

(define-read-only (get-bounty-review (bounty-id uint) (reviewer principal))
    (map-get? bounty-reviews { bounty-id: bounty-id, reviewer: reviewer })
)

(define-read-only (get-user-profile (user principal))
    (map-get? user-profiles { user: user })
)

(define-read-only (get-category-stats (category (string-ascii 32)))
    (map-get? category-stats { category: category })
)

(define-read-only (get-verified-reviewer (reviewer principal))
    (map-get? verified-reviewers { reviewer: reviewer })
)

(define-read-only (get-bounty-dispute (bounty-id uint))
    (map-get? bounty-disputes { bounty-id: bounty-id })
)

(define-read-only (get-contract-info)
    {
        next-bounty-id: (var-get next-bounty-id),
        platform-fee: (var-get platform-fee),
        min-bounty-amount: (var-get min-bounty-amount),
        reviewer-reward: (var-get reviewer-reward),
        is-paused: (var-get contract-paused)
    }
)

(define-read-only (get-bounty-status (bounty-id uint))
    (match (map-get? bounties { bounty-id: bounty-id })
        bounty (some {
            status: (get status bounty),
            current-reviews: (get current-reviews bounty),
            required-reviews: (get required-reviews bounty),
            approved-reviews: (get approved-reviews bounty),
            has-hunter: (is-some (get hunter bounty)),
            is-expired: (< (get deadline bounty) u0)
        })
        none
    )
)

;; Emergency Functions
(define-public (emergency-withdraw)
    (begin
        (asserts! (is-owner) err-owner-only)
        (asserts! (var-get contract-paused) err-unauthorized)
        (as-contract (stx-transfer? (stx-get-balance tx-sender) tx-sender contract-owner))
    )
)