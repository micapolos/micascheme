(import
  (typico library)
  (typico typed)
  (typico core lookup)
  (typico core types)
  (only (micascheme) define-rule-syntax stack datum/annotation))

(define-rule-syntax (check-library-syntax in out)
  (check-library
    (library+syntax (core-lookup) (empty-library) (datum/annotation in))
    out))

; --- library

(check-library
  (empty-library)
  (library (export)))

(check-library
  (library
    (lambda ($lookup) #f)
    (stack
      (typed integer-type 'i)
      (typed string-type 's))
    (stack
      '(define i 10)
      '(define s "foo")))
  (library
    (export
      (i integer)
      (s string))
    (define i 10)
    (define s "foo")))

; --- library+syntax

(check-library-syntax
  (value x 10)
  (library
    (export
      (x integer))
    (define x 10)))

(check-library-syntax
  (function (foo (integer x) (integer y)) (+ x y))
  (library
    (export
      (foo (function (integer integer) integer)))
    (define foo
      (lambda (x y)
        (($primitive 3 +) x y)))))
