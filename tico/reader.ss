(library (tico reader)
  (export
    typings-reader
    read-typings
    read-typing)
  (import
    (micascheme)
    (leo reader)
    (tico typing)
    (tico type))

  (define (top-level-reader $locals $typings $end-fn)
    (reader
      (lambda ($literal)
        (top-level-reader
          $locals
          (push $typings (literal->typing $literal))
          $end-fn))
      (lambda ($symbol)
        (case $symbol
          ((native)
            (top-level-reader $locals (stack)
              (lambda ($native-typings)
                (top-level-reader
                  $locals
                  (push-all $typings
                    (map typing-native $native-typings))
                  $end-fn))))
          ((take)
            (top-level-reader $locals (stack)
              (lambda ($take-typings)
                (top-level-reader
                  $locals
                  (push-all $typings $take-typings)
                  $end-fn))))
          ((with)
            (with-reader $locals (stack)
              (lambda ($with-typings)
                (top-level-reader
                  $locals
                  (push-all $typings $with-typings)
                  $end-fn))))
          ((do)
            (top-level-reader
              (push-all $locals $typings)
              (stack)
              (lambda ($body-typings)
                (top-level-reader
                  $locals
                  $body-typings
                  $end-fn))))
          ((apply)
            (top-level-reader $locals (stack)
              (lambda ($arg-typings)
                (top-level-reader $locals
                  (map
                    (lambda ($typing)
                      (typing-application $typing
                        (reverse $arg-typings)))
                    $typings)
                  $end-fn))))
          ((doing) TODO)
          (else
            (top-level-reader $locals (stack)
              (lambda ($symbol-typings)
                (top-level-reader
                  $locals
                  (push $typings
                    (typing-struct $symbol
                      (reverse $symbol-typings)))
                  $end-fn))))))
      (lambda ()
        ($end-fn $typings))))

  (define (with-reader $locals $typings $end-fn)
    (reader
      (lambda ($literal)
        (with-reader
          $locals
          (push $typings (literal->typing $literal))
          $end-fn))
      (lambda ($symbol)
        (top-level-reader $locals (stack)
          (lambda ($arg-typings)
            (with-reader $locals
              (push $typings
                (typing-struct $symbol
                  (reverse $arg-typings)))
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
