(library (micalang runtime)
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

  (define zero? (lambda (x) (fxzero? x)))
  (define inc (lambda (x) (fx+/wraparound x 1)))
  (define dec (lambda (x) (fx-/wraparound x 1)))

  (define + (lambda (x) (lambda (y) (fx+/wraparound x y))))
  (define - (lambda (x) (lambda (y) (fx-/wraparound x y))))
  (define < (lambda (x) (lambda (y) (fx< x y))))
)
