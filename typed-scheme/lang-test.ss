(import
  (micascheme)
  (typed-scheme lang)
  (typed-scheme type))

(define-type a-null)
(define-type a-boolean)
(define-type a-string)
(define-type a-number)
(define-type (a-pair car cdr))

(check
  (equal?
    (type (oneof))
    (union-type (immutable-vector))))

(check
  (equal?
    (type a-null)
    (defined-type #f (get-type-definition a-null) (immutable-vector))))

(assume-type string-append (union-type (immutable-vector)))
(check (equal? (typeof string-append) (union-type (immutable-vector))))
