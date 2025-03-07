(library (tico reader)
  (export
    bindings-read-typings
    bindings-read-typing
    read-typings
    read-typing
    typing-reader
    args-typing-reader
    typings-reader
    push-block-reader)
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
    (leo reader)
    (tico datum))

  (define-reader (typings-reader $bindings $end)
    (push-block-reader $bindings (empty-block)
      (lambda ($block)
        ($end
          (block-end-typings $block)))))

  (define-reader (typing-reader $bindings $end)
    (push-block-reader $bindings (empty-block)
      (lambda ($block)
        ($end
          (block-let
            (bindings-stack-typing $bindings)
            $block
            (lambda ($scope $typings)
              (single-typing $typings)))))))

  (define-reader (args-typing-reader $bindings $end)
    (push-block-reader $bindings (empty-block)
      (lambda ($block)
        ($end
          (block-let
            (bindings-stack-typing $bindings)
            $block
            (lambda ($scope $typings)
              (typing-args (reverse $typings))))))))

  (define-reader (block-reader $bindings $end)
    (push-block-reader $bindings (empty-block) $end))

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
              (run
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
            (push-natives-reader (stack)
              (lambda ($natives)
                (push-block-reader
                  $bindings
                  (block+typings $block
                    (map native->typing
                      (reverse $natives)))
                  $end))))
          ((as)
            (lets
              ($typing (block-typing $block))
              (typing-reader $bindings
                (lambda ($as-typing)
                  (push-block-reader
                    $bindings
                    (block-updating-typing $block
                      (typing-as $typing $as-typing))
                    $end)))))
          ((assert)
            (typing-reader $bindings
              (lambda ($assert-typing)
                (push-block-reader
                  $bindings
                  (lets
                    (run (typing-assert $assert-typing))
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
          ; TODO: Currently it does not work because we don't support typing-args
          ((then)
            (lets
              ($typings (reverse (block-typings $block)))
              ($parameters (map typing-parameter $typings))
              (typing-reader
                (push-list $bindings (map binding $parameters))
                (lambda ($then-typing)
                  (push-block-reader
                    $bindings
                    (block
                      (push
                        (block-entries $block)
                        (entry $parameters $typings))
                      (push
                        (reverse $parameters)
                        $then-typing))
                    $end)))))
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
                            (switch-exhaustive $typing-opt
                              ((typing? $typing)
                                (bindings-typing-get $bindings $typing $patterns))
                              ((false? _)
                                (bindings-get $bindings $patterns))))))
                      $end))))))
          ((do)
            (lets
              ($constant-typings (reverse (block-typings $block)))
              ($parameter-typings (ordered-map typing-parameter $constant-typings))
              (typing-reader
                (push-list $bindings (map binding $parameter-typings))
                (lambda ($body-typing)
                  (push-block-reader
                    $bindings
                    (block-updating-typing $block
                      (typings-do
                        (bindings-stack-typing $bindings)
                        $parameter-typings
                        $constant-typings
                        (list $body-typing)))
                    $end)))))
          ((apply)
            (lets
              ($typing (block-typing $block))
              (block-reader $bindings
                (lambda ($arg-block)
                  (push-block-reader
                    $bindings
                    (block-updating-typing $block
                      (block-let
                        (bindings-stack-typing $bindings)
                        $arg-block
                        (lambda ($scope $typings)
                          (typing-application $typing
                            (reverse $typings)))))
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
                    (block-updating-typing $block
                      (scope-typing-abstraction
                        (bindings-stack-typing $bindings)
                        $param-typings
                        (list $doing-typing)))
                    $end)))))
          ((promising)
            (typing-reader $bindings
              (lambda ($result-typing)
                (push-block-reader
                  $bindings
                  (block-updating-typing $block
                    (typing-promising
                      (reverse (block-typings $block))
                      (list $result-typing)))
                  $end))))
          ((offering)
            (lets
              ($typing (block-typing $block))
              (typing-reader $bindings
                (lambda ($offering-typing)
                  (push-block-reader
                    $bindings
                    (block-updating-typing $block
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
                    (block-updating-typing $block
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
            (block-reader $bindings
              (lambda ($symbol-block)
                (push-block-reader
                  $bindings
                  (block-updating-typings $block
                    (bindings-resolve $bindings
                      (push
                        (block-typings $block)
                        (block-let
                          (bindings-stack-typing $bindings)
                          $symbol-block
                          (lambda ($scope $typings)
                            (typing-resolve
                              (typing-struct $symbol
                                (reverse $typings))))))))
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

  (define-reader (push-natives-reader $natives $end)
    (reader
      (lambda ($literal)
        (push-natives-reader
          (push $natives
            (string->read-datum
              (ensure string? $literal)))
          $end))
      (lambda ($symbol)
        (throw native-symbol))
      (lambda ()
        ($end $natives))))

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

  (define-rule-syntax (bindings-read-typings $bindings $body ...)
    (reader-eval
      (typings-reader $bindings)
      $body ...))

  (define-rule-syntax (bindings-read-typing $bindings $body ...)
    (reader-eval
      (typing-reader $bindings)
      $body ...))

  (define-rule-syntax (read-typings $body ...)
    (bindings-read-typings (stack) $body ...))

  (define-rule-syntax (read-typing $body ...)
    (bindings-read-typing (stack) $body ...))
)
