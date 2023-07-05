(library (typed)
  (export 
    typed typed? typed-value typed-type
    parse evaluate)

  (import (micascheme) (term) (type))

  (data (typed value type))

  ; ----------------------------------------------------------------

  (define (v $index)
    (string->symbol
      (string-append "v"
        (number->string $index))))

  (define (parse-list $env $stxs)
    (map (partial parse $env) $stxs))

  (define (parse-as $env $stx $as-type)
    (let* (($typed (parse $env $stx))
           ($type (typed-type $typed)))
      (if (matches? $as-type $typed)
        (typed (typed-value $typed) $as-type)
        (syntax-error $stx
          (format "should be ~s, is ~s:" $as-type $type)))))

  (define (parse-proc $env $stx)
    (let* (($typed (parse $env $stx))
           ($type (typed-type $typed)))
      (and (arrow? $type)
        (syntax-error $stx
          (format "should be procedure, is ~s:" $type)))))

  (define (parse $env $stx)
    (syntax-case $stx (lambda)
      ((id arg ...) (identifier? #`id)
        (let* (($id (syntax->datum #`id))
               ($args (syntax->list #`(arg ...)))
               ($typed-args (parse-list $env $args))
               ($arg-types (map typed-type $typed-args))
               ($arg-values (map typed-value $typed-args))
               ($type `(,$id ,@$arg-types))
               ($indexed-result-type 
                (map-find-indexed 
                  (lambda ($env-type) 
                    (and 
                      (arrow? $env-type) 
                      (matches? (arrow-lhs $env-type) $type) 
                      (arrow-rhs $env-type)))
                  $env)))
          (if $indexed-result-type
            (let* (($result-type (indexed-value $indexed-result-type))
                   ($result-index (indexed-index $indexed-result-type))
                   ($var (variable $result-index)))
              (typed 
                (application $var $arg-values)
                $result-type))
            (typed
              (application `list (cons $id $arg-values))
              `(,$id ,@$arg-types)))))
      (_
        (switch (syntax->datum $stx)
          ((boolean? $boolean) 
            (typed $boolean (any-boolean)))
          ((number? $number)
            (typed $number (any-number)))
          ((string? $string) 
            (typed $string (any-string)))
          ((symbol? $symbol)
            (typed $symbol $symbol))))))

  ; --------------------------------------------------------

  (define (evaluate $env $stx)
    (let* (($arity (length $env))
           ($ids (map car $env))
           ($types (map cdr $env))
           ($typed (parse $types $stx))
           ($term (typed-value $typed))
           ($type (typed-type $typed)))
      (typed
        (eval-term
          (application (abstraction $arity $term) (reverse $ids))
          (environment `(micascheme) `(type)))
        $type)))

  (define scheme-env
    (list 
      (cons `string-length (arrow `(length ,(any-string)) (any-number)))
      (cons `number->string (arrow `(string ,(any-number)) (any-string)))
      (cons `string-append (arrow `(append ,(any-string) ,(any-string)) (any-string)))))

)