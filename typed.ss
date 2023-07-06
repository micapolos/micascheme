(library (typed)
  (export 
    typed typed? typed-value typed-type
    parse! parse
    evaluate! evaluate

    ; aux keywords
    boolean number get use)

  (import (micascheme) (term) (type))

  (data (typed value type))

  ; ----------------------------------------------------------------

  (data (env frames))
  (data (frame types))

  (define (frame-symbol->indexed-types $frame $symbol)
    (define $indexed-types
      (map-indexed
        (lambda ($index $type) (indexed $type $index))
        (frame-types $frame)))
    (filter
      (lambda ($indexed-type)
        (eq? (type-selector (indexed-value $indexed-type)) $symbol))
      $indexed-types))

  (define (frame-symbol-ref $frame $symbol)
    (let* (($indexed-types (frame-symbol->indexed-types $frame $symbol))
           ($length (length $indexed-types)))
      (and
        (= $length 1)
        (car $indexed-types))))

  (define (frame-symbol-index-ref $frame $symbol $index)
    (let* (($indexed-types (frame-symbol->indexed-types $frame $symbol))
           ($length (length $indexed-types)))
      (and
        (< $index $length)
        (list-ref (reverse $indexed-types) $index))))

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
      (unless (type-type? (typed-type $typed))
        (syntax-error $stx 
          (format "should be type:")))
      $value))

  (define-aux-keyword boolean)
  (define-aux-keyword number)
  (define-aux-keyword get)
  (define-aux-keyword use)
  (define-aux-keyword type)

  (define (env-parse $env $type? $stx)
    (syntax-case $stx (native boolean number string lambda arrow get use)
      ((native $value $type)
        (if (identifier? #`$value)
          (typed 
            (native (syntax->datum #`$value))
            (env-parse-type $env #`$type))
          (syntax-error #`$value "should be identifier:")))
      (boolean
        (typed (boolean-type) (type-type)))
      (number 
        (typed (number-type) (type-type)))
      (string
        (typed (string-type) (type-type)))
      ((arrow (name param ...) rhs)
        (typed
          (arrow
            (syntax->datum #`name)
            (map (partial env-parse-type $env) (syntax->list #`(param ...)))
            (env-parse-type $env #`rhs))
          (type-type)))
      ((get $type-stx)
        (let* (($type (env-parse-type $env #`$type-stx))
               ($indexed-boolean (map-find-indexed (lambda ($env-type) (matches? $env-type $type)) $env)))
          (if $indexed-boolean
            (typed (variable (indexed-index $indexed-boolean)) $type)
            (syntax-error $stx "unbound"))))
      ((lambda (name param ...) body)
        (let* (($name (syntax->datum #`name))
               ($params (syntax->list #`(param ...)))
               ($body #`body)
               ($param-types (map (partial env-parse-type $env) $params))
               ($arity (length $params))
               ($body-env (append (reverse $param-types) $env))
               ($typed-body (env-parse $body-env $type? $body))
               ($body-term (typed-value $typed-body))
               ($body-type (typed-type $typed-body)))
          (typed
            (abstraction $arity $body-term)
            (arrow $name $param-types $body-type))))
      ((use (expr ...) body)
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
               ($type (tuple-type $id $arg-types))
               ($indexed-result-type 
                (map-find-indexed 
                  (lambda ($env-type) 
                    (and 
                      (arrow? $env-type) 
                      (symbol=? (arrow-name $env-type) $id)
                      (list-matches? (arrow-params $env-type) $arg-types) 
                      (arrow-result $env-type)))
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
                  (tuple-type $id $arg-values)
                  (type-type))
                (typed
                  (make-tuple $arg-values)
                  (tuple-type $id $arg-types)))))))
      (_
        (switch (syntax->datum $stx)
          ((boolean? $boolean) 
            (typed $boolean (boolean-type)))
          ((number? $number)
            (typed $number (number-type)))
          ((string? $string) 
            (typed $string (string-type)))
          ((symbol? $symbol)
            (typed #f $symbol))))))

  ; --------------------------------------------------------

  (define-syntax evaluate!
    (syntax-rules ()
      ((_ expr)
        (evaluate #'expr))))

  (define (evaluate $stx)
    (let* (($typed (parse $stx))
           ($term (typed-value $typed))
           ($type (typed-type $typed)))
      (typed
        (eval-term $term
          (environment `(micascheme) `(term) `(type)))
        $type)))
)
