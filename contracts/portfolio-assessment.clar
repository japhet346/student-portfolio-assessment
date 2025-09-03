;; Student Portfolio Assessment
;; A competency-based evaluation system for tracking student portfolios,
;; project documentation, peer reviews, and skill demonstrations across institutions

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PORTFOLIO-NOT-FOUND (err u101))
(define-constant ERR-PROJECT-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-REVIEWED (err u103))
(define-constant ERR-INVALID-SCORE (err u104))
(define-constant ERR-INSTITUTION-NOT-FOUND (err u105))

;; Data Variables
(define-data-var next-portfolio-id uint u1)
(define-data-var next-project-id uint u1)
(define-data-var next-institution-id uint u1)

;; Data Maps

;; Institution registry
(define-map institutions 
    { institution-id: uint }
    { 
        name: (string-ascii 100),
        admin: principal,
        verified: bool,
        total-students: uint
    }
)

;; Student portfolios
(define-map portfolios
    { portfolio-id: uint }
    {
        student: principal,
        institution-id: uint,
        total-projects: uint,
        avg-score: uint,
        skills-verified: (list 10 (string-ascii 50)),
        created-at: uint,
        active: bool
    }
)

;; Individual projects within portfolios
(define-map projects
    { project-id: uint }
    {
        portfolio-id: uint,
        title: (string-ascii 100),
        description: (string-ascii 500),
        skills-demonstrated: (list 5 (string-ascii 50)),
        submission-hash: (string-ascii 64),
        peer-reviews: uint,
        total-score: uint,
        completed: bool,
        submitted-at: uint
    }
)

;; Peer reviews
(define-map peer-reviews
    { reviewer: principal, project-id: uint }
    {
        score: uint,
        feedback: (string-ascii 500),
        skills-validated: (list 5 (string-ascii 50)),
        reviewed-at: uint
    }
)

;; Student to portfolio mapping
(define-map student-portfolios
    { student: principal }
    { portfolio-id: uint }
)

;; Institution admin mapping
(define-map institution-admins
    { admin: principal }
    { institution-id: uint }
)

;; Public Functions

;; Register a new institution
(define-public (register-institution (name (string-ascii 100)))
    (let
        (
            (institution-id (var-get next-institution-id))
        )
        (map-set institutions
            { institution-id: institution-id }
            {
                name: name,
                admin: tx-sender,
                verified: false,
                total-students: u0
            }
        )
        (map-set institution-admins
            { admin: tx-sender }
            { institution-id: institution-id }
        )
        (var-set next-institution-id (+ institution-id u1))
        (ok institution-id)
    )
)

;; Create a new student portfolio
(define-public (create-portfolio (institution-id uint))
    (let
        (
            (portfolio-id (var-get next-portfolio-id))
        )
        (asserts! (is-some (map-get? institutions { institution-id: institution-id })) ERR-INSTITUTION-NOT-FOUND)
        (map-set portfolios
            { portfolio-id: portfolio-id }
            {
                student: tx-sender,
                institution-id: institution-id,
                total-projects: u0,
                avg-score: u0,
                skills-verified: (list),
                created-at: stacks-block-height,
                active: true
            }
        )
        (map-set student-portfolios
            { student: tx-sender }
            { portfolio-id: portfolio-id }
        )
        (var-set next-portfolio-id (+ portfolio-id u1))
        (ok portfolio-id)
    )
)

;; Submit a new project to portfolio
(define-public (submit-project 
    (portfolio-id uint)
    (title (string-ascii 100))
    (description (string-ascii 500))
    (skills-demonstrated (list 5 (string-ascii 50)))
    (submission-hash (string-ascii 64))
    )
    (let
        (
            (project-id (var-get next-project-id))
            (portfolio-data (unwrap! (map-get? portfolios { portfolio-id: portfolio-id }) ERR-PORTFOLIO-NOT-FOUND))
        )
        (asserts! (is-eq (get student portfolio-data) tx-sender) ERR-NOT-AUTHORIZED)
        (map-set projects
            { project-id: project-id }
            {
                portfolio-id: portfolio-id,
                title: title,
                description: description,
                skills-demonstrated: skills-demonstrated,
                submission-hash: submission-hash,
                peer-reviews: u0,
                total-score: u0,
                completed: false,
                submitted-at: stacks-block-height
            }
        )
        ;; Update portfolio project count
        (map-set portfolios
            { portfolio-id: portfolio-id }
            (merge portfolio-data { total-projects: (+ (get total-projects portfolio-data) u1) })
        )
        (var-set next-project-id (+ project-id u1))
        (ok project-id)
    )
)

;; Submit peer review for a project
(define-public (submit-peer-review 
    (project-id uint)
    (score uint)
    (feedback (string-ascii 500))
    (skills-validated (list 5 (string-ascii 50)))
    )
    (let
        (
            (project-data (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
            (portfolio-data (unwrap! (map-get? portfolios { portfolio-id: (get portfolio-id project-data) }) ERR-PORTFOLIO-NOT-FOUND))
        )
        (asserts! (> score u0) ERR-INVALID-SCORE)
        (asserts! (<= score u100) ERR-INVALID-SCORE)
        (asserts! (not (is-eq (get student portfolio-data) tx-sender)) ERR-NOT-AUTHORIZED)
        (asserts! (is-none (map-get? peer-reviews { reviewer: tx-sender, project-id: project-id })) ERR-ALREADY-REVIEWED)
        
        (map-set peer-reviews
            { reviewer: tx-sender, project-id: project-id }
            {
                score: score,
                feedback: feedback,
                skills-validated: skills-validated,
                reviewed-at: stacks-block-height
            }
        )
        ;; Update project review count and score
        (let
            (
                (new-review-count (+ (get peer-reviews project-data) u1))
                (new-total-score (+ (get total-score project-data) score))
            )
            (map-set projects
                { project-id: project-id }
                (merge project-data 
                    { 
                        peer-reviews: new-review-count,
                        total-score: new-total-score
                    }
                )
            )
        )
        (ok true)
    )
)

;; Verify skills for a portfolio (institution admin only)
(define-public (verify-portfolio-skills 
    (portfolio-id uint)
    (verified-skills (list 10 (string-ascii 50)))
    )
    (let
        (
            (portfolio-data (unwrap! (map-get? portfolios { portfolio-id: portfolio-id }) ERR-PORTFOLIO-NOT-FOUND))
            (admin-data (unwrap! (map-get? institution-admins { admin: tx-sender }) ERR-NOT-AUTHORIZED))
        )
        (asserts! (is-eq (get institution-id portfolio-data) (get institution-id admin-data)) ERR-NOT-AUTHORIZED)
        (map-set portfolios
            { portfolio-id: portfolio-id }
            (merge portfolio-data { skills-verified: verified-skills })
        )
        (ok true)
    )
)

;; Read-only Functions

;; Get portfolio details
(define-read-only (get-portfolio (portfolio-id uint))
    (map-get? portfolios { portfolio-id: portfolio-id })
)

;; Get project details
(define-read-only (get-project (project-id uint))
    (map-get? projects { project-id: project-id })
)

;; Get peer review
(define-read-only (get-peer-review (reviewer principal) (project-id uint))
    (map-get? peer-reviews { reviewer: reviewer, project-id: project-id })
)

;; Get institution details
(define-read-only (get-institution (institution-id uint))
    (map-get? institutions { institution-id: institution-id })
)

;; Get student's portfolio ID
(define-read-only (get-student-portfolio (student principal))
    (map-get? student-portfolios { student: student })
)

;; Calculate project average score
(define-read-only (get-project-average-score (project-id uint))
    (match (map-get? projects { project-id: project-id })
        project-data 
            (if (> (get peer-reviews project-data) u0)
                (some (/ (get total-score project-data) (get peer-reviews project-data)))
                (some u0)
            )
        none
    )
)

;; Get portfolio statistics
(define-read-only (get-portfolio-stats (portfolio-id uint))
    (map-get? portfolios { portfolio-id: portfolio-id })
)
