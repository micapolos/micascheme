(library (tico reader-2)
  (export)
  (import
    (micascheme)
    (leo reader)
    (tico typing))

  (define (top-level-reader $locals $typings $end-fn)
    (reader
      (lambda ($literal)
        (top-level-reader
          $locals
          (push $typings (literal->typing $literal))
          $end-fn))
      (lambda ($symbol)
        (top-level-reader
          $locals
          (stack)
          (lambda ($symbol-typings)
            (top-level-reader
              $locals
              (typing-struct $symbol $symbol-typings)
              $end-fn))))
      (lambda ()
        ($end-fn $typings))))
)
