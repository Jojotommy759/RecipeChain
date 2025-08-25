;; RecipeChain: Culinary Recipe and Cooking Technique Exchange Platform
;; Version: 1.0.0

(define-constant ERR-NOT-AUTHORIZED (err u1))
(define-constant ERR-RECIPE-NOT-FOUND (err u2))
(define-constant ERR-ALREADY-PUBLISHED (err u3))
(define-constant ERR-INVALID-STATUS (err u4))
(define-constant ERR-INVALID-COOK-TIME (err u5))
(define-constant ERR-INVALID-CUISINE (err u6))
(define-constant ERR-INVALID-SKILL-LEVEL (err u7))
(define-constant ERR-INVALID-RECIPE-TITLE (err u8))
(define-constant ERR-INVALID-INSTRUCTIONS (err u9))

(define-constant MIN-COOK-TIME u5)

(define-data-var next-recipe-id uint u1)

(define-map recipe-collection
    uint
    {
        chef: principal,
        recipe-title: (string-utf8 50),
        instructions: (string-utf8 200),
        cuisine: (string-utf8 15),
        skill-level: (string-utf8 10),
        sharing-status: (string-utf8 15),
        cook-time-minutes: uint
    })

(define-private (validate-cuisine (cuisine (string-utf8 15)))
    (or 
        (is-eq cuisine u"Italian")
        (is-eq cuisine u"French")
        (is-eq cuisine u"Asian")
        (is-eq cuisine u"Mexican")
        (is-eq cuisine u"Mediterranean")
        (is-eq cuisine u"American")
    ))

(define-private (validate-skill-level (skill-level (string-utf8 10)))
    (or 
        (is-eq skill-level u"Beginner")
        (is-eq skill-level u"Easy")
        (is-eq skill-level u"Moderate")
        (is-eq skill-level u"Advanced")
        (is-eq skill-level u"Expert")
    ))

(define-private (validate-text-structure (text (string-utf8 200)) (min-length uint) (max-length uint))
    (let 
        (
            (text-length (len text))
        )
        (and 
            (>= text-length min-length)
            (<= text-length max-length)
        )
    ))

(define-public (share-recipe 
    (recipe-title (string-utf8 50))
    (instructions (string-utf8 200))
    (cuisine (string-utf8 15))
    (skill-level (string-utf8 10))
    (cook-time-minutes uint))
    (let
        (
            (recipe-id (var-get next-recipe-id))
        )
        (asserts! (validate-text-structure recipe-title u3 u50) ERR-INVALID-RECIPE-TITLE)
        (asserts! (validate-text-structure instructions u10 u200) ERR-INVALID-INSTRUCTIONS)
        (asserts! (>= cook-time-minutes MIN-COOK-TIME) ERR-INVALID-COOK-TIME)
        (asserts! (validate-cuisine cuisine) ERR-INVALID-CUISINE)
        (asserts! (validate-skill-level skill-level) ERR-INVALID-SKILL-LEVEL)
        
        (map-set recipe-collection recipe-id {
            chef: tx-sender,
            recipe-title: recipe-title,
            instructions: instructions,
            cuisine: cuisine,
            skill-level: skill-level,
            sharing-status: u"public",
            cook-time-minutes: cook-time-minutes
        })
        (var-set next-recipe-id (+ recipe-id u1))
        (ok recipe-id)
    ))

(define-public (make-private-recipe (recipe-id uint))
    (let
        (
            (recipe (unwrap! (map-get? recipe-collection recipe-id) ERR-RECIPE-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get chef recipe)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get sharing-status recipe) u"public") ERR-INVALID-STATUS)
        (ok (map-set recipe-collection recipe-id (merge recipe { sharing-status: u"private" })))
    ))

(define-read-only (get-recipe (recipe-id uint))
    (ok (map-get? recipe-collection recipe-id)))

(define-read-only (get-chef (recipe-id uint))
    (ok (get chef (unwrap! (map-get? recipe-collection recipe-id) ERR-RECIPE-NOT-FOUND))))

(define-read-only (get-total-recipes)
    (ok (- (var-get next-recipe-id) u1)))

(define-read-only (get-sharing-status (recipe-id uint))
    (ok (get sharing-status (unwrap! (map-get? recipe-collection recipe-id) ERR-RECIPE-NOT-FOUND))))