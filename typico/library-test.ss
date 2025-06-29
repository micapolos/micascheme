(import
  (typico library)
  (typico typed)
  (typico core lookup)
  (typico core types)
  (rename (micascheme) (library %library) (library-exports %library-exports)))

; --- library

(check-library
  (empty-library)
  (library (export)))

(check-library
  (library
    identity
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

(check-library+syntax (core-lookup)
  (value x 10)
  (library
    (export
      (x integer))
    (define x 10)))

(check-library+syntax (core-lookup)
  (function (foo (integer x) (integer y)) (+ x y))
  (library
    (export
      (foo (-> integer integer integer)))
    (define foo
      (lambda (x y)
        (($primitive 3 +) x y)))))
