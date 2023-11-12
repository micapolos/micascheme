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
                  (push $typings
                    (typing-native
                      (or-throw (single $native-typings))))
                  $end))))
          ((as)
            (top-level-reader $bindings (stack)
              (lambda ($as-typings)
                (top-level-reader
                  $bindings
                  (stack
                    (typing-as
                      (or-throw (single $typings))
                      (or-throw (single $as-typings))))
                  $end))))
          ((imply)
            (top-level-reader $bindings (stack)
              (lambda ($imply-typings)
                (top-level-reader
                  $bindings
                  (stack
                    (typing-imply
                      (or-throw (single $typings))
                      (or-throw (single $imply-typings))))
                  $end))))
          ((prepare)
            (top-level-reader $bindings (stack)
              (lambda ($prepare-typings)
                (top-level-reader
                  $bindings
                  (push $typings
                    (typing-prepare
                      (or-throw
                        (single $prepare-typings))))
                  $end))))
          ((the)
            (top-level-reader $bindings (stack)
              (lambda ($the-typings)
                (top-level-reader
                  $bindings
                  (push $typings (or-throw (single $the-typings)))
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
              ($parameter-typings (ordered-map typing-parameter $argument-typings))
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
                        (or-throw (single $body-typings))))
                    $end)))))
          ((apply)
            (top-level-reader $bindings (stack)
              (lambda ($arg-typings)
                (top-level-reader
                  $bindings
                  (stack
                    (typing-application
                      (or-throw (single $typings))
                      (reverse $arg-typings)))
                  $end))))
          ((doing)
            (lets
              ($param-types (map typing->type (reverse $typings)))
              ($param-typings (ordered-map generate-parameter-typing $param-types))
              (top-level-reader
                (push-list $bindings (map binding $param-typings))
                (stack)
                (lambda ($doing-typings)
                  (top-level-reader
                    $bindings
                    (stack
                      (typing-abstraction $param-typings
                        (or-throw (single $doing-typings))))
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
                  (stack
                    (typings-offering
                      (reverse $typings)
                      (or-throw (single $offering-typings))))
                  $end))))
          ((type)
            (top-level-reader $bindings (stack)
              (lambda ($type-typings)
                (top-level-reader 
                  $bindings
                  (push $typings
                    (typing->type-typing
                      (or-throw (single $type-typings))))
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
