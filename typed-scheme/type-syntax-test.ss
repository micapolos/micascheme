(import
  (micascheme)
  (syntax lookup)
  (typed-scheme type)
  (typed-scheme type-syntax))

(define boolean-type-definition (type-definition #f (gensym) "boolean" 0))
(define string-type-definition (type-definition #f (gensym) "string" 0))
(define pair-type-definition (type-definition #f (gensym) "pair" 2))

(define $lookup
  (lookup-with
    (a-boolean boolean-type-definition)
    (a-string string-type-definition)
    (a-pair pair-type-definition)))

(check
  (equal?
    (syntax->type $lookup (stack) #'a-boolean)
    (defined-type #f boolean-type-definition (immutable-vector))))

(check
  (equal?
    (syntax->type $lookup (stack) #'(a-pair a-string a-boolean))
    (defined-type #f pair-type-definition
      (immutable-vector
        (defined-type #f string-type-definition (immutable-vector))
        (defined-type #f boolean-type-definition (immutable-vector))))))
