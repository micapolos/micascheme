(library (tico reader)
  (export
    typings-reader
    read-typings
    read-typing)
  (import
    (micascheme)
    (leo reader)
    (tico typing)
    (tico type)
    (tico binding))

  (define (top-level-reader $bindings $typings $end-fn)
    (reader
      (lambda ($literal)
        (top-level-reader
          $bindings
          (push $typings (literal->typing $literal))
          $end-fn))
      (lambda ($symbol)
        (case $symbol
          ((native)
            (top-level-reader $bindings (stack)
              (lambda ($native-typings)
                (top-level-reader
                  $bindings
                  (push-all $typings
                    (map typing-native $native-typings))
                  $end-fn))))
          ((inline)
            (top-level-reader $bindings (stack)
              (lambda ($inline-typings)
                (top-level-reader
                  $bindings
                  (push-all $typings
                    (map typing-inline $inline-typings))
                  $end-fn))))
          ((take)
            (top-level-reader $bindings (stack)
              (lambda ($take-typings)
                (top-level-reader
                  $bindings
                  (push-all $typings $take-typings)
                  $end-fn))))
          ((with)
            (with-reader $bindings (stack)
              (lambda ($with-typings)
                (top-level-reader
                  $bindings
                  (push-all $typings $with-typings)
                  $end-fn))))
          ((get)
            (top-level-reader $bindings (stack)
              (lambda ($get-typings)
                (top-level-reader
                  $bindings
                  (switch $bindings
                    ((null? _)
                      (typings-get $typings $get-typings))
                    ((pair? $bindings)
                      TODO))
                  $end-fn))))
          ((do)
            (top-level-reader
              (push-all $bindings (map typing->binding $typings))
              (stack)
              (lambda ($body-typings)
                (top-level-reader
                  $bindings
                  $body-typings
                  $end-fn))))
          ((apply)
            (top-level-reader $bindings (stack)
              (lambda ($arg-typings)
                (top-level-reader $bindings
                  (map
                    (lambda ($typing)
                      (typing-application $typing
                        (reverse $arg-typings)))
                    $typings)
                  $end-fn))))
          ((doing) TODO)
          ((giving) TODO)
          (else
            (top-level-reader $bindings (stack)
              (lambda ($symbol-typings)
                (top-level-reader
                  $bindings
                  (push $typings
                    (typing-resolve
                      (typing-struct $symbol
                        (reverse $symbol-typings))))
                  $end-fn))))))
      (lambda ()
        ($end-fn $typings))))

  (define (with-reader $bindings $typings $end-fn)
    (reader
      (lambda ($literal)
        (with-reader
          $bindings
          (push $typings (literal->typing $literal))
          $end-fn))
      (lambda ($symbol)
        (top-level-reader $bindings (stack)
          (lambda ($arg-typings)
            (with-reader $bindings
              (push $typings
                (typing-resolve
                  (typing-struct $symbol
                    (reverse $arg-typings))))
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
