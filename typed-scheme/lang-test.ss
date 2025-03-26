(import
  (micascheme)
  (typed-scheme lang)
  (typed-scheme type))

(define-type a-null)
(check (equal? (type a-null) a-null))

(define-type a-boolean)
(check (equal? (type a-boolean) a-boolean))

(define-type a-string)
(check (equal? (type a-string) a-string))

(define-type a-number)
(check (equal? (type a-number) a-number))

(define-type (a-pair car cdr))
(check
  (equal?
    (type (a-pair a-string a-number))
    (a-pair a-string a-number)))

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
