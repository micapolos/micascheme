(library (micalang runtime-term)
  (export bool int inc dec + - < zero?)
  (import
    (only (micascheme) export quote define lambda fx+/wraparound fx-/wraparound fx< fxzero?)
    (micalang term))
  (export
    (import
      (only (micascheme) lambda equal?)
      (micalang term)))

  (define bool (native 'bool))
  (define int (native 'int))

  (define zero? (abstraction (lambda (x) (apply-term fxzero? x))))
  (define inc (abstraction (lambda (x) (apply-term fx+/wraparound x 1))))
  (define dec (abstraction (lambda (x) (apply-term (fx-/wraparound x 1)))))

  (define + (abstraction (lambda (x) (abstraction (lambda (y) (term-apply (apply-term fx+/wraparound x) y))))))
  (define - (abstraction (lambda (x) (abstraction (lambda (y) (term-apply (apply-term fx-/wraparound x) y))))))
  (define < (abstraction (lambda (x) (abstraction (lambda (y) (term-apply (apply-term fx< x) y))))))
)
