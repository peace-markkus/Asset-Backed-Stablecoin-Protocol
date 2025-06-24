;; Asset-Backed Stablecoin Protocol
;; A decentralized stablecoin backed by diversified real-world assets
;; Features: Asset tokenization, reserve auditing, automatic rebalancing

;; ===== CONSTANTS =====

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INSUFFICIENT_BALANCE (err u101))
(define-constant ERR_INVALID_AMOUNT (err u102))
(define-constant ERR_ASSET_NOT_FOUND (err u103))
(define-constant ERR_INSUFFICIENT_COLLATERAL (err u104))
(define-constant ERR_REBALANCE_NOT_NEEDED (err u105))
(define-constant ERR_ORACLE_STALE (err u106))
(define-constant ERR_INVALID_ASSET_TYPE (err u107))
(define-constant ERR_RESERVE_RATIO_VIOLATION (err u108))

;; Stablecoin parameters
(define-constant STABLECOIN_DECIMALS u6)
(define-constant TARGET_PRICE u1000000) ;; $1.00 in 6 decimals
(define-constant MIN_COLLATERAL_RATIO u150) ;; 150% minimum collateralization
(define-constant REBALANCE_THRESHOLD u5) ;; 5% deviation triggers rebalance
(define-constant ORACLE_VALIDITY_PERIOD u144) ;; ~24 hours in blocks
(define-constant MAX_MINT_AMOUNT u1000000000000) ;; 1M tokens max per mint

;; ===== DATA VARIABLES =====

(define-data-var total-supply uint u0)
(define-data-var protocol-admin principal CONTRACT_OWNER)
(define-data-var emergency-shutdown bool false)
(define-data-var last-rebalance-block uint u0)
(define-data-var total-collateral-value uint u0)

;; ===== DATA MAPS =====

;; User balances
(define-map balances principal uint)

;; Asset registry - tracks tokenized real-world assets
(define-map assets
  { asset-id: uint }
  {
    asset-type: (string-ascii 32),
    total-value: uint,
    last-audit-block: uint,
    oracle-price: uint,
    oracle-updated-block: uint,
    weight-percentage: uint,
    is-active: bool
  }
)

;; User asset holdings
(define-map user-assets
  { user: principal, asset-id: uint }
  { amount: uint }
)

;; Collateral positions
(define-map collateral-positions
  principal
  {
    total-deposited: uint,
    total-minted: uint,
    last-update-block: uint
  }
)

;; Asset audit trail
(define-map audit-records
  { asset-id: uint, audit-block: uint }
  {
    auditor: principal,
    verified-value: uint,
    audit-hash: (buff 32)
  }
)

;; ===== READ-ONLY FUNCTIONS =====

(define-read-only (get-balance (user principal))
  (default-to u0 (map-get? balances user))
)

(define-read-only (get-total-supply)
  (var-get total-supply)
)

(define-read-only (get-asset-info (asset-id uint))
  (map-get? assets { asset-id: asset-id })
)

(define-read-only (get-user-asset-balance (user principal) (asset-id uint))
  (default-to u0
    (get amount (map-get? user-assets { user: user, asset-id: asset-id }))
  )
)

(define-read-only (get-collateral-position (user principal))
  (map-get? collateral-positions user)
)

(define-read-only (calculate-collateral-ratio (user principal))
  (let (
    (position (unwrap! (get-collateral-position user) (err u0)))
    (deposited (get total-deposited position))
    (minted (get total-minted position))
  )
    (if (is-eq minted u0)
      (ok u0)
      (ok (/ (* deposited u100) minted))
    )
  )
)

(define-read-only (get-total-collateral-value)
  (var-get total-collateral-value)
)

(define-read-only (calculate-protocol-collateral-ratio)
  (let (
    (total-collateral (var-get total-collateral-value))
    (total-issued (var-get total-supply))
  )
    (if (is-eq total-issued u0)
      u0
      (/ (* total-collateral u100) total-issued)
    )
  )
)

(define-read-only (is-rebalance-needed)
  (let (
    (current-ratio (calculate-protocol-collateral-ratio))
    (target-ratio MIN_COLLATERAL_RATIO)
  )
    (or
      (< current-ratio (- target-ratio REBALANCE_THRESHOLD))
      (> current-ratio (+ target-ratio REBALANCE_THRESHOLD))
    )
  )
)

;; ===== PRIVATE FUNCTIONS =====

(define-private (update-total-collateral-value)
  (let (
    (new-total (fold calculate-asset-value (list u1 u2 u3 u4 u5) u0))
  )
    (var-set total-collateral-value new-total)
    (ok new-total)
  )
)

(define-private (calculate-asset-value (asset-id uint) (acc uint))
  (match (get-asset-info asset-id)
    asset-info
    (if (get is-active asset-info)
      (+ acc (get total-value asset-info))
      acc
    )
    acc
  )
)

(define-private (is-oracle-data-fresh (asset-id uint))
  (match (get-asset-info asset-id)
    asset-info
    (< (- stacks-block-height (get oracle-updated-block asset-info)) ORACLE_VALIDITY_PERIOD)
    false
  )
)

(define-private (transfer-token (from principal) (to principal) (amount uint))
  (let (
    (from-balance (get-balance from))
    (to-balance (get-balance to))
  )
    (asserts! (>= from-balance amount) ERR_INSUFFICIENT_BALANCE)
    (map-set balances from (- from-balance amount))
    (map-set balances to (+ to-balance amount))
    (ok true)
  )
)

;; ===== PUBLIC FUNCTIONS =====

;; Asset Management Functions

(define-public (register-asset (asset-id uint) (asset-type (string-ascii 32)) (initial-value uint) (weight-percentage uint))
  (begin
    (asserts! (is-eq tx-sender (var-get protocol-admin)) ERR_UNAUTHORIZED)
    (asserts! (is-none (get-asset-info asset-id)) ERR_INVALID_ASSET_TYPE)
    (asserts! (> weight-percentage u0) ERR_INVALID_AMOUNT)
    (asserts! (<= weight-percentage u100) ERR_INVALID_AMOUNT)

    (map-set assets
      { asset-id: asset-id }
      {
        asset-type: asset-type,
        total-value: initial-value,
        last-audit-block: stacks-block-height,
        oracle-price: initial-value,
        oracle-updated-block: stacks-block-height,
        weight-percentage: weight-percentage,
        is-active: true
      }
    )

    (unwrap! (update-total-collateral-value) ERR_INVALID_AMOUNT)
    (ok asset-id)
  )
)

(define-public (update-asset-price (asset-id uint) (new-price uint))
  (begin
    (asserts! (is-eq tx-sender (var-get protocol-admin)) ERR_UNAUTHORIZED)
    (asserts! (not (var-get emergency-shutdown)) ERR_UNAUTHORIZED)

    (match (get-asset-info asset-id)
      asset-info
      (begin
        (map-set assets
          { asset-id: asset-id }
          (merge asset-info {
            oracle-price: new-price,
            oracle-updated-block: stacks-block-height,
            total-value: new-price
          })
        )
        (unwrap! (update-total-collateral-value) ERR_INVALID_AMOUNT)
        (ok true)
      )
      ERR_ASSET_NOT_FOUND
    )
  )
)

(define-public (audit-asset (asset-id uint) (verified-value uint) (audit-hash (buff 32)))
  (begin
    (asserts! (is-eq tx-sender (var-get protocol-admin)) ERR_UNAUTHORIZED)

    (match (get-asset-info asset-id)
      asset-info
      (begin
        ;; Record audit
        (map-set audit-records
          { asset-id: asset-id, audit-block: stacks-block-height }
          {
            auditor: tx-sender,
            verified-value: verified-value,
            audit-hash: audit-hash
          }
        )

        ;; Update asset info
        (map-set assets
          { asset-id: asset-id }
          (merge asset-info {
            total-value: verified-value,
            last-audit-block: stacks-block-height
          })
        )

        (unwrap! (update-total-collateral-value) ERR_INVALID_AMOUNT)
        (ok true)
      )
      ERR_ASSET_NOT_FOUND
    )
  )
)

;; Stablecoin Minting and Burning Functions

(define-public (deposit-collateral-and-mint (asset-id uint) (collateral-amount uint) (mint-amount uint))
  (begin
    (asserts! (not (var-get emergency-shutdown)) ERR_UNAUTHORIZED)
    (asserts! (> collateral-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> mint-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (<= mint-amount MAX_MINT_AMOUNT) ERR_INVALID_AMOUNT)

    (match (get-asset-info asset-id)
      asset-info
      (begin
        (asserts! (get is-active asset-info) ERR_ASSET_NOT_FOUND)
        (asserts! (is-oracle-data-fresh asset-id) ERR_ORACLE_STALE)

        ;; Calculate collateral value
        (let (
          (collateral-value (* collateral-amount (get oracle-price asset-info)))
          (required-collateral (* mint-amount MIN_COLLATERAL_RATIO))
          (current-position (default-to
            { total-deposited: u0, total-minted: u0, last-update-block: u0 }
            (get-collateral-position tx-sender)
          ))
        )
          (asserts! (>= (* collateral-value u100) required-collateral) ERR_INSUFFICIENT_COLLATERAL)

          ;; Update user's collateral position
          (map-set collateral-positions tx-sender
            {
              total-deposited: (+ (get total-deposited current-position) collateral-value),
              total-minted: (+ (get total-minted current-position) mint-amount),
              last-update-block: stacks-block-height
            }
          )

          ;; Update user's asset balance
          (let (
            (current-asset-balance (get-user-asset-balance tx-sender asset-id))
          )
            (map-set user-assets
              { user: tx-sender, asset-id: asset-id }
              { amount: (+ current-asset-balance collateral-amount) }
            )
          )

          ;; Mint stablecoins
          (let (
            (current-balance (get-balance tx-sender))
          )
            (map-set balances tx-sender (+ current-balance mint-amount))
            (var-set total-supply (+ (var-get total-supply) mint-amount))
            (ok mint-amount)
          )
        )
      )
      ERR_ASSET_NOT_FOUND
    )
  )
)

(define-public (burn-and-withdraw (burn-amount uint) (asset-id uint) (withdraw-amount uint))
  (begin
    (asserts! (> burn-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= (get-balance tx-sender) burn-amount) ERR_INSUFFICIENT_BALANCE)

    (match (get-collateral-position tx-sender)
      position
      (begin
        (asserts! (>= (get total-minted position) burn-amount) ERR_INSUFFICIENT_BALANCE)

        (let (
          (new-minted (- (get total-minted position) burn-amount))
          (asset-balance (get-user-asset-balance tx-sender asset-id))
          (asset-info (unwrap! (get-asset-info asset-id) ERR_ASSET_NOT_FOUND))
          (withdraw-value (* withdraw-amount (get oracle-price asset-info)))
          (new-deposited (- (get total-deposited position) withdraw-value))
        )
          (asserts! (>= asset-balance withdraw-amount) ERR_INSUFFICIENT_BALANCE)

          ;; Check collateral ratio after withdrawal
          (if (> new-minted u0)
            (asserts! (>= (/ (* new-deposited u100) new-minted) MIN_COLLATERAL_RATIO) ERR_RESERVE_RATIO_VIOLATION)
            true
          )

          ;; Update positions
          (map-set collateral-positions tx-sender
            {
              total-deposited: new-deposited,
              total-minted: new-minted,
              last-update-block: stacks-block-height
            }
          )

          ;; Update asset balance
          (map-set user-assets
            { user: tx-sender, asset-id: asset-id }
            { amount: (- asset-balance withdraw-amount) }
          )

          ;; Burn tokens
          (let (
            (current-balance (get-balance tx-sender))
          )
            (map-set balances tx-sender (- current-balance burn-amount))
            (var-set total-supply (- (var-get total-supply) burn-amount))
            (ok withdraw-amount)
          )
        )
      )
      ERR_ASSET_NOT_FOUND
    )
  )
)

;; Transfer function
(define-public (transfer (recipient principal) (amount uint))
  (begin
    (asserts! (not (var-get emergency-shutdown)) ERR_UNAUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (transfer-token tx-sender recipient amount)
  )
)

;; Rebalancing Functions

(define-public (rebalance-protocol)
  (begin
    (asserts! (is-eq tx-sender (var-get protocol-admin)) ERR_UNAUTHORIZED)
    (asserts! (is-rebalance-needed) ERR_REBALANCE_NOT_NEEDED)

    (unwrap! (update-total-collateral-value) ERR_INVALID_AMOUNT)
    (var-set last-rebalance-block stacks-block-height)

    (ok (calculate-protocol-collateral-ratio))
  )
)

(define-public (liquidate-position (user principal))
  (begin
    (asserts! (not (var-get emergency-shutdown)) ERR_UNAUTHORIZED)

    (match (get-collateral-position user)
      position
      (let (
        (collateral-ratio (unwrap! (calculate-collateral-ratio user) ERR_INVALID_AMOUNT))
      )
        (asserts! (< collateral-ratio MIN_COLLATERAL_RATIO) ERR_RESERVE_RATIO_VIOLATION)

        ;; Liquidation logic would go here
        ;; For now, we'll just clear the position
        (map-delete collateral-positions user)
        (ok true)
      )
      ERR_ASSET_NOT_FOUND
    )
  )
)

;; Admin Functions

(define-public (set-emergency-shutdown (shutdown bool))
  (begin
    (asserts! (is-eq tx-sender (var-get protocol-admin)) ERR_UNAUTHORIZED)
    (var-set emergency-shutdown shutdown)
    (ok shutdown)
  )
)

(define-public (set-protocol-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender (var-get protocol-admin)) ERR_UNAUTHORIZED)
    (var-set protocol-admin new-admin)
    (ok new-admin)
  )
)

;; Initialize protocol with default assets
(define-public (initialize-protocol)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)

    ;; Register initial asset types (examples)
    (unwrap! (register-asset u1 "REAL_ESTATE" u100000000 u40) ERR_INVALID_AMOUNT)
    (unwrap! (register-asset u2 "COMMODITIES" u50000000 u30) ERR_INVALID_AMOUNT)
    (unwrap! (register-asset u3 "BONDS" u30000000 u20) ERR_INVALID_AMOUNT)
    (unwrap! (register-asset u4 "STOCKS" u20000000 u10) ERR_INVALID_AMOUNT)

    (ok true)
  )
)
