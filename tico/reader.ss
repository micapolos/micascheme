(library (tico reader)
  (export
    bindings-read-typings
    bindings-read-typing
    read-typings
    read-typing
    typing-reader)
  (import
    (micascheme)
    (leo reader)
    (tico typing)
    (tico type)
    (tico binding)
    (tico path)
    (tico block)
    (leo parser)
    (leo reader))

  (define-reader (typings-reader $bindings $end)
    (push-block-reader $bindings (empty-block)
      (lambda ($block)
        ($end
          (block-end-typings $block)))))

  (define-reader (typing-reader $bindings $end)
    (push-block-reader $bindings (empty-block)
      (lambda ($block)
        ($end
          (block-let $block
            (lambda ($typings)
              (or-throw (single $typings))))))))

  (define-reader (push-block-reader $bindings $block $end)
    (reader
      (lambda ($literal)
        (push-block-reader
          $bindings
          (block+typing $block (literal->typing $literal))
          $end))
      (lambda ($symbol)
        (case $symbol
          ((load)
            (paths-reader
              (lambda ($paths)
                (reader-read-list
                  (push-block-reader $bindings $block $end)
                  (flatten (map load-script (map path-filename $paths)))))))
          ((native)
            (typing-reader $bindings
              (lambda ($native-typing)
                (push-block-reader
                  $bindings
                  (block+typing $block
                    (typing-native $native-typing))
                  $end))))
          ((as)
            (lets
              ($typing (block-typing $block))
              (typing-reader $bindings
                (lambda ($as-typing)
                  (push-block-reader
                    $bindings
                    (block-with-typing $block
                      (typing-as $typing $as-typing))
                    $end)))))
          ((assert)
            (typing-reader $bindings
              (lambda ($assert-typing)
                (push-block-reader
                  $bindings
                  (lets
                    (do (typing-assert $assert-typing))
                    $block)
                  $end))))
          ((prepare)
            (typing-reader $bindings
              (lambda ($prepare-typing)
                (push-block-reader
                  $bindings
                  (block+typing $block
                    (typing-prepare $prepare-typing))
                  $end))))
          ((the)
            (typing-reader $bindings
              (lambda ($the-typing)
                (push-block-reader
                  $bindings
                  (block+typing $block $the-typing)
                  $end))))
          ((with)
            (push-with-typings-reader $bindings (stack)
              (lambda ($with-typings)
                (push-block-reader
                  $bindings
                  (block+typings $block (reverse $with-typings))
                  $end))))
          ((get)
            (typings-reader $bindings
              (lambda ($get-typings)
                (push-block-reader
                  $bindings
                  (block-update-typings $block
                    (lambda ($block-typings)
                    (switch $block-typings
                      ((null? _)
                        (bindings-get* $bindings $get-typings))
                      ((pair? $block-typings)
                        (stack (typings-get $block-typings $get-typings))))))
                  $end))))
          ((do)
            (lets
              ($argument-typings (reverse (block-typings $block)))
              ($parameter-typings (ordered-map typing-parameter $argument-typings))
              (typing-reader
                (push-list $bindings (map binding $parameter-typings))
                (lambda ($body-typing)
                  (push-block-reader
                    $bindings
                    (block-with-typing $block
                      (typings-do
                        $parameter-typings
                        $argument-typings
                        $body-typing))
                    $end)))))
          ((apply)
            (lets
              ($typing (block-typing $block))
              (typings-reader $bindings
                (lambda ($arg-typings)
                  (push-block-reader
                    $bindings
                    (block-with-typing $block
                      (typing-application $typing
                        (reverse $arg-typings)))
                    $end)))))
          ((doing)
            (lets
              ($param-types (map typing->type (reverse (block-typings $block))))
              ($param-typings (ordered-map generate-parameter-typing $param-types))
              (typing-reader
                (push-list $bindings (map binding $param-typings))
                (lambda ($doing-typing)
                  (push-block-reader
                    $bindings
                    (block-with-typing $block
                      (typing-abstraction $param-typings $doing-typing))
                    $end)))))
          ((promising)
            (typing-reader $bindings
              (lambda ($result-typing)
                (push-block-reader
                  $bindings
                  (block-with-typing $block
                    (typing-promising
                      (reverse (block-typings $block))
                      $result-typing))
                  $end))))
          ((offering)
            (typing-reader $bindings
              (lambda ($offering-typing)
                (push-block-reader
                  $bindings
                  (block-with-typing $block
                    (typing-offering
                      (reverse (block-typings $block))
                      $offering-typing))
                  $end))))
          ((type)
            (typing-reader $bindings
              (lambda ($type-typing)
                (push-block-reader
                  $bindings
                  (block+typing $block
                    (typing->type-typing $type-typing))
                  $end))))
          ((comment)
            (comment-reader
              (lambda ($commented)
                (push-block-reader
                  $bindings
                  $block
                  $end))))
          (else
            (typings-reader $bindings
              (lambda ($symbol-typings)
                (push-block-reader
                  $bindings
                  (block-with-typings $block
                    (bindings-resolve $bindings
                      (push
                        (block-typings $block)
                        (typing-resolve
                          (typing-struct $symbol
                            (reverse $symbol-typings))))))
                  $end))))))
      (lambda ()
        ($end $block))))

  (define-reader (push-with-typings-reader $bindings $typings $end)
    (reader
      (lambda ($literal)
        (push-with-typings-reader
          $bindings
          (push $typings (literal->typing $literal))
          $end))
      (lambda ($symbol)
        (typings-reader $bindings
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

  (define-syntax-rule (bindings-read-typings $bindings $body ...)
    (reader-eval
      (push-block-reader $bindings (empty-block) block-end-typings)
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
