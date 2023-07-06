(library (typed)
  (export 
    typed typed? typed-value typed-type
    parse! parse
    evaluate! evaluate

    ; aux keywords
    boolean number get type)

  (import (micascheme) (term) (type))

  (data (typed value type))

  ; ----------------------------------------------------------------

  (define-syntax parse!
    (syntax-rules ()
      ((_ expr)
        (parse #'expr))))

  (define (parse $stx)
    (env-parse (list) #f $stx))

  (define (v $index)
    (string->symbol
      (string-append "v"
        (number->string $index))))

  (define (env-parse-list $env $type? $stxs)
    (map (partial env-parse $env $type?) $stxs))

  (define (env-parse-as $env $type? $stx $as-type)
    (let* (($typed (env-parse $env $type? $stx))
           ($type (typed-type $typed)))
      (if (matches? $as-type $typed)
        (typed (typed-value $typed) $as-type)
        (syntax-error $stx
          (format "should be ~s, is ~s:" $as-type $type)))))

  (define (env-parse-proc $env $type? $stx)
    (let* (($typed (env-parse $env $type? $stx))
           ($type (typed-type $typed)))
      (and (arrow? $type)
        (syntax-error $stx
          (format "should be procedure, is ~s:" $type)))))

  (define (env-parse-type $env $stx)
    (let* (($typed (env-parse $env #t $stx))
           ($value (typed-value $typed))
           ($type (typed-type $typed)))
      (unless (any-type? (typed-type $typed))
        (syntax-error $stx 
          (format "should be type:")))
      $value))

  (define-aux-keyword boolean)
  (define-aux-keyword number)
  (define-aux-keyword get)
  (define-aux-keyword type)

  (define (env-parse $env $type? $stx)
    (syntax-case $stx (native type boolean number string arrow get let)
      ((native $value $type)
        (if (identifier? #`$value)
          (typed 
            (native (syntax->datum #`$value))
            (env-parse-type $env #`$type))
          (syntax-error #`$value "should be identifier:")))
      ((type expr)
        (typed (env-parse-type $env #`expr) (any-type)))
      (boolean
        (typed (any-boolean) (any-type)))
      (number 
        (typed (any-number) (any-type)))
      (string
        (typed (any-string) (any-type)))
      ((arrow lhs rhs)
        (typed
          (arrow 
            (env-parse-type $env #`lhs)
            (env-parse-type $env #`rhs))
          (any-type)))
      ((get $type-stx)
        (let* (($type (env-parse-type $env #`$type-stx))
               ($indexed-boolean (map-find-indexed (lambda ($env-type) (matches? $env-type $type)) $env)))
          (if $indexed-boolean
            (typed (variable (indexed-index $indexed-boolean)) $type)
            (syntax-error $stx "unbound"))))
      ((let (expr ...) body)
        (let* (($exprs (syntax->list #`(expr ...)))
               ($body #`body)
               ($arity (length $exprs))
               ($typed-terms (env-parse-list $env $type? $exprs))
               ($terms (map typed-value $typed-terms))
               ($types (map typed-type $typed-terms))
               ($body-env (append (reverse $types) $env))
               ($typed-body (env-parse $body-env $type? $body))
               ($body-term (typed-value $typed-body))
               ($body-type (typed-type $typed-body)))
          (typed
            (application (abstraction $arity $body-term) $terms)
            $body-type)))
      ((id arg ...) (identifier? #`id)
        (let* (($id (syntax->datum #`id))
               ($args (syntax->list #`(arg ...)))
               ($typed-args (env-parse-list $env $type? $args))
               ($arg-types (map typed-type $typed-args))
               ($arg-values (map typed-value $typed-args))
               ($type (any-tuple $id $arg-types))
               ($indexed-result-type 
                (map-find-indexed 
                  (lambda ($env-type) 
                    (and 
                      (arrow? $env-type) 
                      (matches? (arrow-lhs $env-type) $type) 
                      (arrow-rhs $env-type)))
                  $env)))
          (cond
            ($indexed-result-type
              (let* (($result-type (indexed-value $indexed-result-type))
                     ($result-index (indexed-index $indexed-result-type))
                     ($var (variable $result-index)))
                (typed 
                  (application $var $arg-values)
                  $result-type)))
            (else
              (if $type?
                (typed
                  (any-tuple $id $arg-values)
                  (any-type))
                (typed
                  (make-tuple $arg-types $arg-values)
                  (any-tuple $id $arg-types)))))))
      (_
        (switch (syntax->datum $stx)
          ((boolean? $boolean) 
            (typed $boolean (any-boolean)))
          ((number? $number)
            (typed $number (any-number)))
          ((string? $string) 
            (typed $string (any-string)))
          ((symbol? $symbol)
            (typed #f $symbol))))))

  ; --------------------------------------------------------

  (define-syntax evaluate!
    (syntax-rules ()
      ((_ expr)
        (evaluate #'expr))))

  (define (evaluate $stx)
    (env-evaluate (list) $stx))

  (define (env-evaluate $env $stx)
    (let* (($arity (length $env))
           ($ids (map car $env))
           ($types (map cdr $env))
           ($typed (env-parse $types #f $stx))
           ($term (typed-value $typed))
           ($type (typed-type $typed)))
      (typed
        (eval-term
          (application (abstraction $arity $term) (reverse $ids))
          (environment `(micascheme) `(type)))
        $type)))
)
