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

  (define-reader (typing-reader $bindings $end)
    (push-typings-reader $bindings (stack)
      (lambda ($typings)
        ($end (or-throw (single $typings))))))

  (define-reader (push-typings-reader $bindings $typings $end)
    (reader
      (lambda ($literal)
        (push-typings-reader
          $bindings
          (push $typings (literal->typing $literal))
          $end))
      (lambda ($symbol)
        (case $symbol
          ((load)
            (paths-reader
              (lambda ($paths)
                (reader-read-list
                  (push-typings-reader $bindings $typings $end)
                  (flatten (map load-script (map path-filename $paths)))))))
          ((native)
            (typing-reader $bindings
              (lambda ($native-typing)
                (push-typings-reader
                  $bindings
                  (push $typings
                    (typing-native $native-typing))
                  $end))))
          ((as)
            (lets
              ($typing (or-throw (single $typings)))
              (typing-reader $bindings
                (lambda ($as-typing)
                  (push-typings-reader
                    $bindings
                    (stack (typing-as $typing $as-typing))
                    $end)))))
          ((assert)
            (typing-reader $bindings
              (lambda ($assert-typing)
                (push-typings-reader
                  $bindings
                  (lets
                    (do (typing-assert $assert-typing))
                    $typings)
                  $end))))
          ((prepare)
            (typing-reader $bindings
              (lambda ($prepare-typing)
                (push-typings-reader
                  $bindings
                  (push $typings
                    (typing-prepare $prepare-typing))
                  $end))))
          ((the)
            (typing-reader $bindings
              (lambda ($the-typing)
                (push-typings-reader
                  $bindings
                  (push $typings $the-typing)
                  $end))))
          ((with)
            (push-with-typings-reader $bindings (stack)
              (lambda ($with-typings)
                (push-typings-reader
                  $bindings
                  (push-all $typings $with-typings)
                  $end))))
          ((get)
            (push-typings-reader $bindings (stack)
              (lambda ($get-typings)
                (push-typings-reader
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
              ($parameter-typings (ordered-map typing-parameter $argument-typings))
              (typing-reader
                (push-list $bindings (map binding $parameter-typings))
                (lambda ($body-typing)
                  (push-typings-reader
                    $bindings
                    (stack
                      (typings-do
                        $parameter-typings
                        $argument-typings
                        $body-typing))
                    $end)))))
          ((apply)
            (lets
              ($typing (or-throw (single $typings)))
              (push-typings-reader $bindings (stack)
                (lambda ($arg-typings)
                  (push-typings-reader
                    $bindings
                    (stack
                      (typing-application $typing
                        (reverse $arg-typings)))
                    $end)))))
          ((doing)
            (lets
              ($param-types (map typing->type (reverse $typings)))
              ($param-typings (ordered-map generate-parameter-typing $param-types))
              (typing-reader
                (push-list $bindings (map binding $param-typings))
                (lambda ($doing-typing)
                  (push-typings-reader
                    $bindings
                    (stack
                      (typing-abstraction $param-typings $doing-typing))
                    $end)))))
          ((promising)
            (typing-reader $bindings
              (lambda ($result-typing)
                (push-typings-reader
                  $bindings
                  (stack
                    (typings-promising $typings $result-typing))
                  $end))))
          ((offering)
            (typing-reader $bindings
              (lambda ($offering-typing)
                (push-typings-reader
                  $bindings
                  (stack
                    (typings-offering
                      (reverse $typings)
                      $offering-typing))
                  $end))))
          ((type)
            (typing-reader $bindings
              (lambda ($type-typing)
                (push-typings-reader
                  $bindings
                  (push $typings
                    (typing->type-typing $type-typing))
                  $end))))
          ((comment)
            (comment-reader
              (lambda ($commented)
                (push-typings-reader
                  $bindings
                  $typings
                  $end))))
          (else
            (push-typings-reader $bindings (stack)
              (lambda ($symbol-typings)
                (push-typings-reader
                  $bindings
                  (bindings-resolve $bindings
                    (push $typings
                      (typing-resolve
                        (typing-struct $symbol (reverse $symbol-typings)))))
                  $end))))))
      (lambda ()
        ($end $typings))))

  (define-reader (push-with-typings-reader $bindings $typings $end)
    (reader
      (lambda ($literal)
        (push-with-typings-reader
          $bindings
          (push $typings (literal->typing $literal))
          $end))
      (lambda ($symbol)
        (push-typings-reader $bindings (stack)
          (lambda ($arg-typings)
            (push-with-typings-reader
              $bindings
              (push $typings
                (typing-resolve
                  (typing-struct $symbol
                    (reverse $arg-typings))))
              $end))))
      (lambda ()
        ($end $typings))))

  (define-reader (comment-reader $end)
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
    (push-typings-reader (stack) (stack) identity))

  (define-syntax-rule (bindings-read-typings $bindings $body ...)
    (reader-eval
      (push-typings-reader $bindings (stack) identity)
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
