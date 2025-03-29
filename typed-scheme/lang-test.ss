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

; (check
;   (equal?
;     (type a-null)
;     (union-type (immutable-vector))))

