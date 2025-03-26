(import
  (micascheme)
  (typed-scheme lang)
  (typed-scheme type))

(define-type a-boolean)
(define-type a-string)
(define-type a-number)

(check (equal? (type a-boolean) a-boolean))
(check (equal? (type a-string) a-string))
(check (equal? (type a-number) a-number))

(check
  (equal?
    (type (a-lambda (a-boolean a-string) a-number))
    (lambda-type
      (immutable-vector a-boolean a-string)
      a-number)))

(check
  (equal?
    (type (oneof a-boolean a-string))
    (union-type
      (immutable-vector a-boolean a-string))))

(check
  (equal?
    (type (forall (i o) (a-lambda (i) o)))
    (forall-type 2
      (lambda-type
        (immutable-vector (variable-type 1))
        (variable-type 0)))))
