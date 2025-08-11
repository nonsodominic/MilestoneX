;; Milestone-based Project Execution Platform
;; A revolutionary decentralized milestone-based project execution platform
;; Enables secure project funding with automated milestone releases and neutral arbitration
;; Built for the future of distributed work collaboration

;; Constants
(define-constant platform-admin tx-sender)
(define-constant err-admin-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-invalid-state (err u104))
(define-constant err-insufficient-balance (err u105))
(define-constant err-invalid-distribution (err u106))
(define-constant err-invalid-parameters (err u107))
(define-constant err-invalid-mediator (err u108))

;; Project execution tracking
(define-data-var project-counter uint u0)

;; Core Data Structures
(define-map projects 
    { project-id: uint }
    {
        initiator: principal,
        executor: (optional principal),
        total-budget: uint,
        milestone-count: uint,
        project-specification: (string-utf8 500),
        execution-state: (string-ascii 20),
        neutral-mediator: principal,
        inception-block: uint
    }
)

(define-map milestones
    { project-id: uint, milestone-id: uint }
    {
        budget-allocation: uint,
        milestone-specification: (string-utf8 256),
        execution-state: (string-ascii 20),
        deliverable-evidence: (optional (string-utf8 500))
    }
)

(define-map project-treasury
    { project-id: uint }
    { 
        secured-funds: uint,
        released-funds: uint
    }
)

(define-map mediations
    { project-id: uint }
    {
        initiated-by: principal,
        conflict-description: (string-utf8 500),
        mediation-fee: uint,
        resolution-complete: bool
    }
)

;; Input validation utilities
(define-private (validate-specification (input (string-utf8 500)))
    (and (> (len input) u0) (<= (len input) u500))
)

(define-private (validate-milestone-spec (input (string-utf8 256)))
    (and (> (len input) u0) (<= (len input) u256))
)

(define-private (validate-mediator (mediator principal))
    (not (is-eq mediator tx-sender))
)

(define-private (project-exists (project-id uint))
    (is-some (map-get? projects {project-id: project-id}))
)

;; Read-only interface functions
(define-read-only (get-project-details (project-id uint))
    (begin
        (asserts! (project-exists project-id) err-not-found)
        (ok (unwrap! (map-get? projects {project-id: project-id}) err-not-found))
    )
)

(define-read-only (get-milestone-details (project-id uint) (milestone-id uint))
    (begin
        (asserts! (project-exists project-id) err-not-found)
        (ok (unwrap! (map-get? milestones {project-id: project-id, milestone-id: milestone-id}) err-not-found))
    )
)

(define-read-only (get-treasury-status (project-id uint))
    (begin
        (asserts! (project-exists project-id) err-not-found)
        (ok (unwrap! (map-get? project-treasury {project-id: project-id}) err-not-found))
    )
)

(define-read-only (get-current-project-count)
    (ok (var-get project-counter))
)

;; Core platform functions
(define-public (initialize-project (project-specification (string-utf8 500)) (total-budget uint) (milestone-count uint) (neutral-mediator principal))
    (let
        (
            (project-id (+ (var-get project-counter) u1))
            (validated-specification project-specification)
            (confirmed-mediator neutral-mediator)
        )
        (asserts! (> total-budget u0) err-invalid-parameters)
        (asserts! (> milestone-count u0) err-invalid-parameters)
        (asserts! (validate-specification validated-specification) err-invalid-parameters)
        (asserts! (validate-mediator confirmed-mediator) err-invalid-mediator)
        
        (try! (stx-transfer? total-budget tx-sender (as-contract tx-sender)))
        
        (map-set projects
            {project-id: project-id}
            {
                initiator: tx-sender,
                executor: none,
                total-budget: total-budget,
                milestone-count: milestone-count,
                project-specification: validated-specification,
                execution-state: "available",
                neutral-mediator: confirmed-mediator,
                inception-block: block-height
            }
        )
        
        (map-set project-treasury
            {project-id: project-id}
            {
                secured-funds: total-budget,
                released-funds: u0
            }
        )
        
        (var-set project-counter project-id)
        (ok project-id)
    )
)

(define-public (claim-project (project-id uint))
    (let
        (
            (validated-project-id project-id)
        )
        (asserts! (project-exists validated-project-id) err-not-found)
        (let
            (
                (project (unwrap! (map-get? projects {project-id: validated-project-id}) err-not-found))
            )
            (asserts! (is-eq (get execution-state project) "available") err-invalid-state)
            (asserts! (is-none (get executor project)) err-already-exists)
            
            (map-set projects
                {project-id: validated-project-id}
                (merge project {
                    executor: (some tx-sender),
                    execution-state: "active"
                })
            )
            (ok true)
        )
    )
)

(define-public (deliver-milestone (project-id uint) (milestone-id uint) (deliverable-evidence (string-utf8 500)))
    (let
        (
            (validated-project-id project-id)
            (validated-milestone-id milestone-id)
            (validated-evidence deliverable-evidence)
        )
        (asserts! (project-exists validated-project-id) err-not-found)
        (asserts! (validate-specification validated-evidence) err-invalid-parameters)
        
        (let
            (
                (project (unwrap! (map-get? projects {project-id: validated-project-id}) err-not-found))
                (milestone (unwrap! (map-get? milestones {project-id: validated-project-id, milestone-id: validated-milestone-id}) err-not-found))
            )
            (asserts! (is-eq (some tx-sender) (get executor project)) err-unauthorized)
            (asserts! (is-eq (get execution-state milestone) "awaiting") err-invalid-state)
            
            (map-set milestones
                {project-id: validated-project-id, milestone-id: validated-milestone-id}
                (merge milestone {
                    execution-state: "delivered",
                    deliverable-evidence: (some validated-evidence)
                })
            )
            (ok true)
        )
    )
)

(define-public (approve-milestone (project-id uint) (milestone-id uint))
    (let
        (
            (validated-project-id project-id)
            (validated-milestone-id milestone-id)
        )
        (asserts! (project-exists validated-project-id) err-not-found)
        
        (let
            (
                (project (unwrap! (map-get? projects {project-id: validated-project-id}) err-not-found))
                (milestone (unwrap! (map-get? milestones {project-id: validated-project-id, milestone-id: validated-milestone-id}) err-not-found))
                (treasury (unwrap! (map-get? project-treasury {project-id: validated-project-id}) err-not-found))
            )
            (asserts! (is-eq tx-sender (get initiator project)) err-unauthorized)
            (asserts! (is-eq (get execution-state milestone) "delivered") err-invalid-state)
            
            ;; Execute payment release
            (try! (as-contract (stx-transfer? 
                (get budget-allocation milestone)
                tx-sender
                (unwrap! (get executor project) err-not-found)
            )))
            
            ;; Update milestone and treasury records
            (map-set milestones
                {project-id: validated-project-id, milestone-id: validated-milestone-id}
                (merge milestone {execution-state: "approved"})
            )
            
            (map-set project-treasury
                {project-id: validated-project-id}
                {
                    secured-funds: (- (get secured-funds treasury) (get budget-allocation milestone)),
                    released-funds: (+ (get released-funds treasury) (get budget-allocation milestone))
                }
            )
            
            (ok true)
        )
    )
)

(define-public (initiate-mediation (project-id uint) (conflict-description (string-utf8 500)))
    (let
        (
            (validated-project-id project-id)
            (validated-description conflict-description)
        )
        (asserts! (project-exists validated-project-id) err-not-found)
        (asserts! (validate-specification validated-description) err-invalid-parameters)
        
        (let
            (
                (project (unwrap! (map-get? projects {project-id: validated-project-id}) err-not-found))
                (mediation-fee (/ (get total-budget project) u20)) ;; 5% mediation fee
            )
            (asserts! (or 
                (is-eq tx-sender (get initiator project))
                (is-eq (some tx-sender) (get executor project))
            ) err-unauthorized)
            
            (map-set mediations
                {project-id: validated-project-id}
                {
                    initiated-by: tx-sender,
                    conflict-description: validated-description,
                    mediation-fee: mediation-fee,
                    resolution-complete: false
                }
            )
            
            (map-set projects
                {project-id: validated-project-id}
                (merge project {execution-state: "mediation"})
            )
            
            (ok true)
        )
    )
)

(define-public (execute-mediation-resolution 
    (project-id uint) 
    (initiator-allocation uint)
    (executor-allocation uint))
    (let
        (
            (validated-project-id project-id)
        )
        (asserts! (project-exists validated-project-id) err-not-found)
        (asserts! (is-eq (+ initiator-allocation executor-allocation) u100) err-invalid-distribution)
        
        (let
            (
                (project (unwrap! (map-get? projects {project-id: validated-project-id}) err-not-found))
                (treasury (unwrap! (map-get? project-treasury {project-id: validated-project-id}) err-not-found))
                (mediation (unwrap! (map-get? mediations {project-id: validated-project-id}) err-not-found))
            )
            (asserts! (is-eq tx-sender (get neutral-mediator project)) err-unauthorized)
            
            ;; Calculate final distributions
            (let
                (
                    (remaining-balance (get secured-funds treasury))
                    (initiator-amount (/ (* remaining-balance initiator-allocation) u100))
                    (executor-amount (/ (* remaining-balance executor-allocation) u100))
                )
                ;; Execute fund distributions
                (try! (as-contract (stx-transfer? initiator-amount tx-sender (get initiator project))))
                (try! (as-contract (stx-transfer? executor-amount tx-sender (unwrap! (get executor project) err-not-found))))
                (try! (as-contract (stx-transfer? (get mediation-fee mediation) tx-sender (get neutral-mediator project))))
                
                ;; Update project status
                (map-set projects
                    {project-id: validated-project-id}
                    (merge project {execution-state: "resolved"})
                )
                
                (map-set mediations
                    {project-id: validated-project-id}
                    (merge mediation {resolution-complete: true})
                )
                
                (ok true)
            )
        )
    )
)

;; Milestone configuration
(define-public (configure-milestone 
    (project-id uint) 
    (milestone-id uint)
    (budget-allocation uint)
    (milestone-specification (string-utf8 256)))
    (let
        (
            (validated-project-id project-id)
            (validated-milestone-id milestone-id)
            (validated-allocation budget-allocation)
            (validated-specification milestone-specification)
        )
        (asserts! (project-exists validated-project-id) err-not-found)
        (asserts! (> validated-allocation u0) err-invalid-parameters)
        (asserts! (validate-milestone-spec validated-specification) err-invalid-parameters)
        
        (let
            (
                (project (unwrap! (map-get? projects {project-id: validated-project-id}) err-not-found))
            )
            (asserts! (is-eq tx-sender (get initiator project)) err-unauthorized)
            (asserts! (< validated-milestone-id (get milestone-count project)) err-invalid-parameters)
            
            (map-set milestones
                {project-id: validated-project-id, milestone-id: validated-milestone-id}
                {
                    budget-allocation: validated-allocation,
                    milestone-specification: validated-specification,
                    execution-state: "awaiting",
                    deliverable-evidence: none
                }
            )
            (ok true)
        )
    )
)