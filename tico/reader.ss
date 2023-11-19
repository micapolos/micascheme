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
    (tico entry)
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
          ((use)
            (lets
              (do
                (unless
                  (null? (block-typings $block))
                  (throw can-not-use)))
              (typing-reader $bindings
                (lambda ($use-typing)
                  (lets
                    ($parameter (typing-parameter $use-typing))
                    (push-block-reader
                      (push $bindings
                        (binding $parameter))
                      (block+entry $block
                        (entry
                          (list $parameter)
                          (list $use-typing)))
                      $end))))))
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
            (lets
              ($typing-opt
                (switch (block-typings $block)
                  ((null? _) #f)
                  ((pair? $pair) (or-throw (single $pair)))))
              (typings-reader $bindings
                (lambda ($get-typings)
                  (lets
                    ($patterns (reverse (map typing->type $get-typings)))
                    (push-block-reader
                      $bindings
                      (block-update-typings $block
                        (lambda ($block-typings)
                          (stack
                            (switch-exclusive $typing-opt
                              ((typing? $typing)
                                (bindings-typing-get $bindings $typing $patterns))
                              ((false? _)
                                (bindings-get $bindings $patterns))))))
                      $end))))))
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
                      (scope-typing-abstraction
                        (bindings-typing-scope $bindings)
                        $param-typings
                        $doing-typing))
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
            (lets
              ($typing (block-typing $block))
              (typing-reader $bindings
                (lambda ($offering-typing)
                  (push-block-reader
                    $bindings
                    (block-with-typing $block
                      (typing-offering
                        $typing
                        $offering-typing))
                    $end)))))
          ((being)
            (lets
              ($typing (block-typing $block))
              (typing-reader $bindings
                (lambda ($being-typing)
                  (push-block-reader
                    $bindings
                    (block-with-typing $block
                      (typing-being
                        $typing
                        $being-typing))
                    $end)))))
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
      (typings-reader $bindings)
      $body ...))

  (define-syntax-rule (bindings-read-typing $bindings $body ...)
    (reader-eval
      (typing-reader $bindings)
      $body ...))

  (define-syntax-rule (read-typings $body ...)
    (bindings-read-typings (stack) $body ...))

  (define-syntax-rule (read-typing $body ...)
    (bindings-read-typing (stack) $body ...))
)
