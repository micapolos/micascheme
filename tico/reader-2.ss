(library (tico reader-2)
  (export
    typings-reader
    read-typings
    read-typing)
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
              (push $typings
                (typing-struct $symbol
                  (reverse $symbol-typings)))
              $end-fn))))
      (lambda ()
        ($end-fn $typings))))

  (define typings-reader
    (top-level-reader (stack) (stack) identity))

  (define-syntax-rule (read-typings $body ...)
    (reader-eval
      typings-reader
      $body ...))

  (define-syntax-rule (read-typing $body ...)
    (car
      (ensure single?
        (read-typings $body ...))))
)
