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

  (define recursive-top-level-reader
    (rec $recurse
      (lambda ($type? $bindings $end-fn)
        (top-level-reader $type? $bindings (stack) $recurse $end-fn))))

  (define recursive-top-level-type-reader
    (rec $recurse
      (lambda ($bindings $end-fn)
        (top-level-reader #t $bindings (stack) $recurse $end-fn))))

  (define (top-level-reader $type? $bindings $typings $recurse $end-fn)
    (reader
      (lambda ($literal)
        (top-level-reader
          $type?
          $bindings
          (push $typings (literal->typing $literal))
          $recurse
          $end-fn))
      (lambda ($symbol)
        (case $symbol
          ((native)
            ($recurse $type? $bindings
              (lambda ($native-typings)
                (top-level-reader
                  $type?
                  $bindings
                  (push-all $typings
                    (map typing-native $native-typings)) 
                  $recurse
                  $end-fn))))
          ((as)
            ($recurse $type? $bindings
              (lambda ($as-typings)
                (top-level-reader
                  $type?
                  $bindings
                  (map typing-as $typings $as-typings)
                  $recurse
                  $end-fn))))
          ((prepare)
            ($recurse $type? $bindings
              (lambda ($prepare-typings)
                (top-level-reader
                  $bindings
                  $type?
                  (push-all $typings
                    (map typing-prepare $prepare-typings))
                  $recurse
                  $end-fn))))
          ((take)
            ($recurse $type? $bindings
              (lambda ($take-typings)
                (top-level-reader
                  $type?
                  $bindings
                  (push-all $typings $take-typings)
                  $recurse
                  $end-fn))))
          ((with)
            (with-reader $type? $bindings (stack) $recurse
              (lambda ($with-typings)
                (top-level-reader
                  $type?
                  $bindings
                  (push-all $typings $with-typings)
                  $recurse
                  $end-fn))))
          ((get)
            ($recurse $type? $bindings
              (lambda ($get-typings)
                (top-level-reader
                  $type?
                  $bindings
                  (switch $typings
                    ((null? _)
                      (bindings-get* $bindings $get-typings))
                    ((pair? $typings)
                      (stack (typings-get $typings $get-typings))))
                  $recurse
                  $end-fn))))
          ((do)
            ($recurse
              $type?
              (push-all $bindings (map typing->binding $typings))
              (lambda ($body-typings)
                (top-level-reader
                  $type?
                  $bindings
                  $body-typings
                  $recurse
                  $end-fn))))
          ((apply)
            ($recurse $type? $bindings
              (lambda ($arg-typings)
                (top-level-reader
                  $type?
                  $bindings
                  (map
                    (lambda ($typing)
                      (typing-application $typing
                        (reverse $arg-typings)))
                    $typings)
                  $recurse
                  $end-fn))))
          ((doing)
            TODO)
          ((promising)
            ($recurse $type? $bindings
              (lambda ($result-typings)
                (top-level-reader 
                  $type?
                  $bindings
                  (stack
                    (typings-promising $typings
                      (car (ensure single? $result-typings))))
                  $recurse
                  $end-fn))))
          ((offering)
            ($recurse $type? $bindings
              (lambda ($offering-typings)
                (top-level-reader 
                  $type?
                  $bindings
                  (typings-offering $typings $offering-typings)
                  $recurse
                  $end-fn))))
          ((type)
            ($recurse $type? $bindings
              (lambda ($type-typings)
                (top-level-reader 
                  #t
                  $bindings
                  (push-all $typings 
                    (map typing->type-typing $type-typings))
                  $recurse
                  $end-fn))))
          ((comment)
            (comment-reader
              (lambda ($commented)
                (top-level-reader
                  $type?
                  $bindings
                  $typings
                  $recurse
                  $end-fn))))
          (else
            ($recurse $type? $bindings
              (lambda ($symbol-typings)
                (top-level-reader
                  $type?
                  $bindings
                  (bindings-resolve $bindings
                    (push $typings
                      (typing-resolve
                        (app
                          (if $type? typing-type-struct typing-struct)
                          $symbol
                          (reverse $symbol-typings)))))
                  $recurse
                  $end-fn))))))
      (lambda ()
        ($end-fn $typings))))

  (define (with-reader $type? $bindings $typings $recurse $end-fn)
    (reader
      (lambda ($literal)
        (with-reader
          $type?
          $bindings
          (push $typings (literal->typing $literal))
          $recurse
          $end-fn))
      (lambda ($symbol)
        ($recurse $type? $bindings
          (lambda ($arg-typings)
            (with-reader 
              $type?
              $bindings
              (push $typings
                (typing-resolve
                  (typing-struct $symbol
                    (reverse $arg-typings))))
              $recurse
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
    (recursive-top-level-reader #f (stack) identity))

  (define-syntax-rule (read-typings $body ...)
    (reader-eval
      typings-reader
      $body ...))

  (define-syntax-rule (read-typing $body ...)
    (car
      (ensure single?
        (read-typings $body ...))))
)
