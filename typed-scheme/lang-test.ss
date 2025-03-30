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

(check
  (equal?
    (type (a-lambda (a-string a-boolean) a-number))
    (lambda-type 0
      (immutable-vector
        (defined-type #f (get-type-definition a-string) (immutable-vector))
        (defined-type #f (get-type-definition a-boolean) (immutable-vector)))
      (defined-type #f (get-type-definition a-number) (immutable-vector)))))

(assume-type string-append (a-lambda (a-string a-string) a-string))
(check
  (equal?
    (typeof string-append)
    (type (a-lambda (a-string a-string) a-string))))
