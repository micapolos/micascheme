(library (typed)
  (export 
    typed typed? typed-value typed-type
    parse! parse
    evaluate! evaluate

    ; aux keywords
    boolean number use type)

  (import (micascheme) (term) (type))

  (data (typed value type))

  ; ----------------------------------------------------------------

  (data (env frames))
  (data (frame types))
  (data (phase depth))

  (define (phase-n? $phase $depth)
    (= (phase-depth $phase) $depth))

  (define (env-select-indexed-type $env $symbol)
    (map-find-indexed 
      (lambda ($type) (and (type-named? $type $symbol) $type))
      $env))

  (define (env-function-type-indexed-type $env $symbol $arg-types)
    (map-find-indexed 
      (lambda ($type) 
        (and 
          (function-type? $type) 
          (symbol=? (function-type-name $type) $symbol)
          (list-matches? (function-type-params $type) $arg-types) 
          (function-type-result $type)))
      $env))

  ; ----------------------------------------------------------------

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
    (env-parse (list) (phase 0) $stx))

  (define (v $index)
    (string->symbol
      (string-append "v"
        (number->string $index))))

  (define (env-parse-list $env $phase $stxs)
    (map (partial env-parse $env $phase) $stxs))

  (define (env-parse-as $env $phase $stx $as-type)
    (let* (($typed (env-parse $env $phase $stx))
           ($type (typed-type $typed)))
      (if (matches? $as-type $typed)
        (typed (typed-value $typed) $as-type)
        (syntax-error $stx
          (format "should be ~s, is ~s:" $as-type $type)))))

  (define (env-parse-proc $env $phase $stx)
    (let* (($typed (env-parse $env $phase $stx))
           ($type (typed-type $typed)))
      (and (function-type? $type)
        (syntax-error $stx
          (format "should be procedure, is ~s:" $type)))))

  (define (env-parse-type $env $stx)
    (let* (($typed (env-parse $env (phase 1) $stx))
           ($value (typed-value $typed))
           ($type (typed-type $typed)))
      (unless (and (universe? $type) (= (universe-depth $type) 0))
        (syntax-error $stx 
          (format "should be universe 0:")))
      $value))

  (define-aux-keyword boolean)
  (define-aux-keyword number)
  (define-aux-keyword use)
  (define-aux-keyword type)

  (define (env-parse $env $phase $stx)
    (syntax-case $stx (native boolean number string function function-type use type)
      ((native $value $type)
        (if (identifier? #`$value)
          (typed 
            (native (syntax->datum #`$value))
            (env-parse-type $env #`$type))
          (syntax-error #`$value "should be identifier:")))
      ((type expr)
        (typed (env-parse-type $env #`expr) type!))
      (boolean (phase-n? $phase 1)
        (typed boolean! type!))
      (number (phase-n? $phase 1)
        (typed number! type!))
      (string (phase-n? $phase 1)
        (typed string! type!))
      (type (phase-n? $phase 1)
        (typed type! type!))
      ((function (name param ...) rhs) (phase-n? $phase 1)
        (typed
          (function-type
            (syntax->datum #`name)
            (map (partial env-parse-type $env) (syntax->list #`(param ...)))
            (env-parse-type $env #`rhs))
          type!))
      ((function (name param ...) body)
        (let* (($name (syntax->datum #`name))
               ($params (syntax->list #`(param ...)))
               ($body #`body)
               ($param-types (map (partial env-parse-type $env) $params))
               ($arity (length $params))
               ($body-env (append (reverse $param-types) $env))
               ($typed-body (env-parse $body-env $phase $body))
               ($body-term (typed-value $typed-body))
               ($body-type (typed-type $typed-body)))
          (typed
            (function $arity $body-term)
            (function-type $name $param-types $body-type))))
      ((use (expr ...) body)
        (let* (($exprs (syntax->list #`(expr ...)))
               ($body #`body)
               ($arity (length $exprs))
               ($typed-terms (env-parse-list $env $phase $exprs))
               ($terms (map typed-value $typed-terms))
               ($types (map typed-type $typed-terms))
               ($body-env (append (reverse $types) $env))
               ($typed-body (env-parse $body-env $phase $body))
               ($body-term (typed-value $typed-body))
               ($body-type (typed-type $typed-body)))
          (typed
            (application (function $arity $body-term) $terms)
            $body-type)))
      ((id arg ...) (identifier? #`id)
        (let* (($id (syntax->datum #`id))
               ($args (syntax->list #`(arg ...)))
               ($typed-args (env-parse-list $env $phase $args))
               ($arg-types (map typed-type $typed-args))
               ($arg-values (map typed-value $typed-args))
               ($type (tuple-type $id $arg-types))
               ($indexed-type (env-function-type-indexed-type $env $id $arg-types)))
          (cond
            ($indexed-type
              (let* (($type (indexed-value $indexed-type))
                     ($index (indexed-index $indexed-type))
                     ($var (variable $index)))
                (typed 
                  (application $var $arg-values)
                  $type)))
            (else
              (if (phase-n? $phase 1)
                (typed
                  (tuple-type $id $arg-values)
                  (universe 0))
                (typed
                  (tuple $arg-values)
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
            (let* (($indexed-type (env-select-indexed-type $env $symbol)))
              (if $indexed-type
                (typed 
                  (variable (indexed-index $indexed-type))
                  (indexed-value $indexed-type))
                (typed #f $symbol))))))))

  ; --------------------------------------------------------

  (define-syntax evaluate!
    (syntax-rules ()
      ((_ expr)
        (evaluate (phase 0) #'expr))))

  (define (evaluate $phase $stx)
    (let* (($typed (env-parse (list) $phase $stx))
           ($term (typed-value $typed))
           ($type (typed-type $typed)))
      (typed
        (eval-term $term
          (environment `(micascheme) `(term) `(type)))
        $type)))
)
