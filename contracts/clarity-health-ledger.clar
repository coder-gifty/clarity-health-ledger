;; Distributed Clarity Health Information Ledger
;;
;; =========================================================
;; SECTION 1: System Constants and Error Definitions
;; =========================================================

;; Platform administration configuration
(define-constant platform-administrator tx-sender) ;; Administrative authority (deployer)

;; Response codes for operational failures
(define-constant STATUS_CONTENT_OVERSIZED (err u304))     ;; Content exceeds size limitations
(define-constant STATUS_CREDENTIALS_INVALID (err u305))   ;; Authentication credentials invalid
(define-constant STATUS_PROVIDER_UNRECOGNIZED (err u306)) ;; Healthcare provider not recognized
(define-constant STATUS_ADMIN_RESTRICTED (err u300))      ;; Administrative privileges required
(define-constant STATUS_DOCUMENT_MISSING (err u301))      ;; Document identifier not in system
(define-constant STATUS_DOCUMENT_DUPLICATE (err u302))    ;; Document already registered
(define-constant STATUS_IDENTIFIER_MALFORMED (err u303))  ;; Malformed identifier provided
(define-constant STATUS_CATEGORY_INVALID (err u307))      ;; Invalid category designator
(define-constant STATUS_ACCESS_VIOLATION (err u308))      ;; Access policy violation

;; =========================================================
;; SECTION 2: Global State Variables
;; =========================================================

;; System-wide document counter
(define-data-var document-counter uint u0) ;; Tracks total documents in system

;; =========================================================
;; SECTION 3: Data Structure Definitions
;; =========================================================

;; Primary document storage structure
(define-map health-documents
  { document-identifier: uint }
  {
    subject-identifier: (string-ascii 64),  ;; Subject's identification code
    provider-address: principal,           ;; Provider's blockchain address
    document-volume: uint,                 ;; Document size in bytes
    timestamp: uint,                       ;; System timestamp (block height)
    clinical-summary: (string-ascii 128),  ;; Clinical observations
    categories: (list 10 (string-ascii 32)) ;; Document classification tags
  }
)

;; Authorization registry for document access
(define-map document-authorizations
  { document-identifier: uint, requestor-address: principal }
  { authorization-status: bool } ;; Whether access is permitted
)

;; =========================================================
;; SECTION 4: Internal Utility Functions
;; =========================================================


;; Performs validation on individual category tag
(define-private (validate-category-format (category-tag (string-ascii 32)))
  (and 
    (> (len category-tag) u0)
    (< (len category-tag) u33)
  )
)

;; Performs comprehensive validation on category collection
(define-private (validate-category-collection (category-collection (list 10 (string-ascii 32))))
  (and
    (> (len category-collection) u0)  ;; Minimum one category required
    (<= (len category-collection) u10) ;; Maximum ten categories allowed
    (is-eq (len (filter validate-category-format category-collection)) (len category-collection)) ;; All categories must pass validation
  )
)

;; Validates existence of document in system
(define-private (document-registered? (document-identifier uint))
  (is-some (map-get? health-documents { document-identifier: document-identifier }))
)

;; Confirms provider ownership of document
(define-private (validate-provider-authority? (document-identifier uint) (provider-address principal))
  (match (map-get? health-documents { document-identifier: document-identifier })
    document-metadata (is-eq (get provider-address document-metadata) provider-address)
    false
  )
)

;; Retrieves document volume information
(define-private (query-document-volume (document-identifier uint))
  (default-to u0
    (get document-volume
      (map-get? health-documents { document-identifier: document-identifier })
    )
  )
)
;; =========================================================
;; SECTION 5: External Interface Functions
;; =========================================================

;; Registers new health document with subject information
(define-public (register-health-document 
  (subject-identifier (string-ascii 64))       ;; Subject's unique identifier 
  (document-volume uint)                     ;; Volume of the document in bytes
  (clinical-summary (string-ascii 128))      ;; Clinical observations and notes
  (categories (list 10 (string-ascii 32)))   ;; Classification categories
)
  (let
    (
      (document-identifier (+ (var-get document-counter) u1))  ;; Generate sequential identifier
    )
    ;; Input validation procedures
    (asserts! (> (len subject-identifier) u0) STATUS_IDENTIFIER_MALFORMED)  ;; Subject ID cannot be empty
    (asserts! (< (len subject-identifier) u65) STATUS_IDENTIFIER_MALFORMED) ;; Subject ID length constraint
    (asserts! (> document-volume u0) STATUS_CONTENT_OVERSIZED)         ;; Volume must be positive
    (asserts! (< document-volume u1000000000) STATUS_CONTENT_OVERSIZED) ;; Volume upper bound enforced
    (asserts! (> (len clinical-summary) u0) STATUS_IDENTIFIER_MALFORMED)  ;; Summary required
    (asserts! (< (len clinical-summary) u129) STATUS_IDENTIFIER_MALFORMED) ;; Summary length constraint
    (asserts! (validate-category-collection categories) STATUS_CATEGORY_INVALID) ;; Category validation

    ;; Document registration in primary storage
    (map-insert health-documents
      { document-identifier: document-identifier }
      {
        subject-identifier: subject-identifier,
        provider-address: tx-sender,  ;; Current transaction sender is registered provider
        document-volume: document-volume,
        timestamp: block-height,  ;; Current block height as timestamp
        clinical-summary: clinical-summary,
        categories: categories
      }
    )

    ;; Establish initial authorization for provider
    (map-insert document-authorizations
      { document-identifier: document-identifier, requestor-address: tx-sender }
      { authorization-status: true }
    )

    ;; Update system document count
    (var-set document-counter document-identifier)
    (ok document-identifier)  ;; Return the newly assigned identifier
  )
)

;; Updates provider association for existing document
(define-public (reassign-document-provider (document-identifier uint) (new-provider-address principal))
  (let
    (
      (document-metadata (unwrap! (map-get? health-documents { document-identifier: document-identifier }) STATUS_DOCUMENT_MISSING)) ;; Retrieve document metadata
    )
    ;; Authorization and existence verification
    (asserts! (document-registered? document-identifier) STATUS_DOCUMENT_MISSING)  ;; Verify document exists
    (asserts! (is-eq (get provider-address document-metadata) tx-sender) STATUS_CREDENTIALS_INVALID) ;; Verify authorization

    ;; Update provider designation in document record
    (map-set health-documents
      { document-identifier: document-identifier }
      (merge document-metadata { provider-address: new-provider-address })
    )
    (ok true)  ;; Confirmation of successful update
  )
)

;; Retrieves document classification categories
(define-public (retrieve-document-categories (document-identifier uint))
  (let
    (
      (document-metadata (unwrap! (map-get? health-documents { document-identifier: document-identifier }) STATUS_DOCUMENT_MISSING)) ;; Retrieve document metadata
    )
    ;; Return classification categories for document
    (ok (get categories document-metadata))
  )
)

;; Queries provider information for specific document
(define-public (query-document-provider (document-identifier uint))
  (let
    (
      (document-metadata (unwrap! (map-get? health-documents { document-identifier: document-identifier }) STATUS_DOCUMENT_MISSING)) ;; Retrieve document metadata
    )
    ;; Return provider address associated with document
    (ok (get provider-address document-metadata))
  )
)

;; Retrieves document creation timestamp
(define-public (query-document-timestamp (document-identifier uint))
  (let
    (
      (document-metadata (unwrap! (map-get? health-documents { document-identifier: document-identifier }) STATUS_DOCUMENT_MISSING)) ;; Retrieve document metadata
    )
    ;; Return timestamp (block height) of document creation
    (ok (get timestamp document-metadata))
  )
)

;; Reports total document count in system
(define-public (query-system-document-count)
  ;; Returns system-wide document counter value
  (ok (var-get document-counter))
)

;; Retrieves size information for specific document
(define-public (query-document-volume-by-id (document-identifier uint))
  (let
    (
      (document-metadata (unwrap! (map-get? health-documents { document-identifier: document-identifier }) STATUS_DOCUMENT_MISSING)) ;; Retrieve document metadata
    )
    ;; Return document volume in bytes
    (ok (get document-volume document-metadata))
  )
)

;; Retrieves clinical summary for document
(define-public (query-document-clinical-summary (document-identifier uint))
  (let
    (
      (document-metadata (unwrap! (map-get? health-documents { document-identifier: document-identifier }) STATUS_DOCUMENT_MISSING)) ;; Retrieve document metadata
    )
    ;; Return clinical observations associated with document
    (ok (get clinical-summary document-metadata))
  )
)

;; Verifies access privileges for specific user
(define-public (verify-authorization-status (document-identifier uint) (requestor-address principal))
  (let
    (
      (authorization-metadata (unwrap! (map-get? document-authorizations { document-identifier: document-identifier, requestor-address: requestor-address }) STATUS_ACCESS_VIOLATION)) ;; Retrieve authorization data
    )
    ;; Return authorization status for requestor
    (ok (get authorization-status authorization-metadata))
  )
)

;; Updates metadata for existing document
(define-public (update-document-metadata 
  (document-identifier uint)                      ;; Target document identifier
  (new-subject-identifier (string-ascii 64))      ;; Updated subject identifier
  (new-document-volume uint)                      ;; Updated document volume
  (new-clinical-summary (string-ascii 128))       ;; Updated clinical summary
  (new-categories (list 10 (string-ascii 32)))    ;; Updated classification categories
)
  (let
    (
      (document-metadata (unwrap! (map-get? health-documents { document-identifier: document-identifier }) STATUS_DOCUMENT_MISSING)) ;; Retrieve current metadata
    )
    ;; Comprehensive validation checks
    (asserts! (document-registered? document-identifier) STATUS_DOCUMENT_MISSING)  ;; Verify document exists
    (asserts! (is-eq (get provider-address document-metadata) tx-sender) STATUS_CREDENTIALS_INVALID)  ;; Verify authorization
    (asserts! (> (len new-subject-identifier) u0) STATUS_IDENTIFIER_MALFORMED)  ;; Subject ID cannot be empty
    (asserts! (< (len new-subject-identifier) u65) STATUS_IDENTIFIER_MALFORMED) ;; Subject ID length constraint
    (asserts! (> new-document-volume u0) STATUS_CONTENT_OVERSIZED)                ;; Volume must be positive
    (asserts! (< new-document-volume u1000000000) STATUS_CONTENT_OVERSIZED)       ;; Volume upper bound enforced
    (asserts! (> (len new-clinical-summary) u0) STATUS_IDENTIFIER_MALFORMED)      ;; Summary required
    (asserts! (< (len new-clinical-summary) u129) STATUS_IDENTIFIER_MALFORMED)    ;; Summary length constraint
    (asserts! (validate-category-collection new-categories) STATUS_CATEGORY_INVALID) ;; Category validation

    ;; Update document metadata record
    (map-set health-documents
      { document-identifier: document-identifier }
      (merge document-metadata { 
        subject-identifier: new-subject-identifier, 
        document-volume: new-document-volume, 
        clinical-summary: new-clinical-summary, 
        categories: new-categories 
      })
    )
    (ok true)  ;; Confirmation of successful update
  )
)

