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
          ((as)
            (top-level-reader $bindings (stack)
              (lambda ($as-typings)
                (top-level-reader
                  $bindings
                  (map typing-as $typings $as-typings)
                  $end-fn))))
          ((prepare)
            (top-level-reader $bindings (stack)
              (lambda ($prepare-typings)
                (top-level-reader
                  $bindings
                  (push-all $typings
                    (map typing-prepare $prepare-typings))
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
                  (switch $typings
                    ((null? _)
                      (bindings-get* $bindings $get-typings))
                    ((pair? $typings)
                      (stack (typings-get $typings $get-typings))))
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
          ((doing)
            TODO)
          ((promising)
            (top-level-reader $bindings (stack)
              (lambda ($result-typings)
                (top-level-reader $bindings
                  (stack
                    (typings-promising $typings
                      (car (ensure single? $result-typings))))
                  $end-fn))))
          ((offering)
            (top-level-reader $bindings (stack)
              (lambda ($offering-typings)
                (top-level-reader $bindings
                  (typings-offering $typings $offering-typings)
                  $end-fn))))
          ((type)
            (top-level-reader $bindings (stack)
              (lambda ($type-typings)
                (top-level-reader $bindings
                  (push-all $typings 
                    (map typing->type-typing $type-typings))
                  $end-fn))))
          ((comment)
            (comment-reader
              (lambda (_)
                (top-level-reader $bindings $typings $end-fn))))
          (else
            (top-level-reader $bindings (stack)
              (lambda ($symbol-typings)
                (top-level-reader
                  $bindings
                  (bindings-resolve $bindings
                    (push $typings
                      (typing-resolve
                        (typing-struct $symbol
                          (reverse $symbol-typings)))))
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

  (define (comment-reader $end-fn)
    (reader
      (lambda ($literal) 
        (comment-reader $end-fn))
      (lambda ($symbol)
        (comment-reader
          (lambda (_)
            (comment-reader $end-fn))))
      (lambda ()
        ($end-fn #f))))

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
