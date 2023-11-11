(library (tico reader)
  (export
    typings-reader
    bindings-read-typings
    bindings-read-typing
    read-typings
    read-typing)
  (import
    (micascheme)
    (leo reader)
    (tico typing)
    (tico type)
    (tico binding)
    (tico path)
    (leo parser)
    (leo reader))

  (define (top-level-reader $bindings $typings $end)
    (reader
      (lambda ($literal)
        (top-level-reader
          $bindings
          (push $typings (literal->typing $literal))
          $end))
      (lambda ($symbol)
        (case $symbol
          ((load)
            (paths-reader
              (lambda ($paths)
                (reader-read-list
                  (top-level-reader $bindings $typings $end)
                  (flatten (map load-script (map path-filename $paths)))))))
          ((native)
            (top-level-reader $bindings (stack)
              (lambda ($native-typings)
                (top-level-reader
                  $bindings
                  (push-all $typings
                    (map typing-native $native-typings)) 
                  $end))))
          ((as)
            (top-level-reader $bindings (stack)
              (lambda ($as-typings)
                (top-level-reader
                  $bindings
                  (map typing-as $typings $as-typings)
                  $end))))
          ((assert)
            (top-level-reader $bindings (stack)
              (lambda ($assert-typings)
                (top-level-reader
                  $bindings
                  (typings-resolve-assert $typings $assert-typings)
                  $end))))
          ((prepare)
            (top-level-reader $bindings (stack)
              (lambda ($prepare-typings)
                (top-level-reader
                  $bindings
                  (push-all $typings
                    (map typing-prepare $prepare-typings))
                  $end))))
          ((take)
            (top-level-reader $bindings (stack)
              (lambda ($take-typings)
                (top-level-reader
                  $bindings
                  (push-all $typings $take-typings)
                  $end))))
          ((with)
            (with-reader $bindings (stack)
              (lambda ($with-typings)
                (top-level-reader
                  $bindings
                  (push-all $typings $with-typings)
                  $end))))
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
                  $end))))
          ((do)
            (lets
              ($argument-typings (reverse $typings))
              ($parameter-typings (map typing-parameter $argument-typings))
              (top-level-reader
                (push-list $bindings (map binding $parameter-typings))
                (stack)
                (lambda ($body-typings)
                  (top-level-reader
                    $bindings
                    (stack
                      (typings-do
                        $parameter-typings
                        $argument-typings
                        (force-single $body-typings)))
                    $end)))))
          ((apply)
            (top-level-reader $bindings (stack)
              (lambda ($arg-typings)
                (top-level-reader
                  $bindings
                  (map
                    (lambda ($typing)
                      (typing-application $typing
                        (reverse $arg-typings)))
                    $typings)
                  $end))))
          ((doing)
            (lets
              ($param-types (map typing->type (reverse $typings)))
              ($param-typings (map generate-parameter-typing $param-types))
              (top-level-reader
                (push-list $bindings (map binding $param-typings))
                (stack)
                (lambda ($doing-typings)
                  (top-level-reader
                    $bindings
                    (stack
                      (typing-abstraction $param-typings
                        (force-single $doing-typings)))
                    $end)))))
          ((promising)
            (top-level-reader $bindings (stack)
              (lambda ($result-typings)
                (top-level-reader 
                  $bindings
                  (stack
                    (typings-promising $typings
                      (car (ensure single? $result-typings))))
                  $end))))
          ((offering)
            (top-level-reader $bindings (stack)
              (lambda ($offering-typings)
                (top-level-reader 
                  $bindings
                  (typings-offering $typings $offering-typings)
                  $end))))
          ((type)
            (top-level-reader $bindings (stack)
              (lambda ($type-typings)
                (top-level-reader 
                  $bindings
                  (push-all $typings 
                    (map typing->type-typing $type-typings))
                  $end))))
          ((comment)
            (comment-reader
              (lambda ($commented)
                (top-level-reader
                  $bindings
                  $typings
                  $end))))
          (else
            (top-level-reader $bindings (stack)
              (lambda ($symbol-typings)
                (top-level-reader
                  $bindings
                  (bindings-resolve $bindings
                    (push $typings
                      (typing-resolve
                        (typing-struct $symbol (reverse $symbol-typings)))))
                  $end))))))
      (lambda ()
        ($end $typings))))

  (define (with-reader $bindings $typings $end)
    (reader
      (lambda ($literal)
        (with-reader
          $bindings
          (push $typings (literal->typing $literal))
          $end))
      (lambda ($symbol)
        (top-level-reader $bindings (stack)
          (lambda ($arg-typings)
            (with-reader 
              $bindings
              (push $typings
                (typing-resolve
                  (typing-struct $symbol
                    (reverse $arg-typings))))
              $end))))
      (lambda ()
        ($end $typings))))

  (define (comment-reader $end)
    (reader
      (lambda ($literal) 
        (comment-reader $end))
      (lambda ($symbol)
        (comment-reader
          (lambda (_)
            (comment-reader $end))))
      (lambda ()
        ($end #f))))

  (define typings-reader
    (top-level-reader (stack) (stack) identity))

  (define-syntax-rule (bindings-read-typings $bindings $body ...)
    (reader-eval
      (top-level-reader $bindings (stack) identity)
      $body ...))

  (define-syntax-rule (bindings-read-typing $bindings $body ...)
    (car
      (ensure single?
        (bindings-read-typings $bindings $body ...))))

  (define-syntax-rule (read-typings $body ...)
    (bindings-read-typings (stack) $body ...))

  (define-syntax-rule (read-typing $body ...)
    (bindings-read-typing (stack) $body ...))
)
