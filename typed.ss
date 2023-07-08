(library (typed)
  (export 
    parse! parse
    evaluate! evaluate

    ; aux keywords
    boolean number use type)

  (import (micascheme) (term) (type))

  (define-aux-keyword boolean)
  (define-aux-keyword number)
  (define-aux-keyword use)
  (define-aux-keyword type)

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
    (lets 
      ($indexed-types (frame-symbol->indexed-types $frame $symbol))
      ($length (length $indexed-types))
      (and
        (= $length 1)
        (car $indexed-types))))

  (define (frame-symbol-index-ref $frame $symbol $index)
    (lets 
      ($indexed-types (frame-symbol->indexed-types $frame $symbol))
      ($length (length $indexed-types))
      (and
        (< $index $length)
        (list-ref (reverse $indexed-types) $index))))

  (define (env-resolve-application $env $symbol $types $terms)
    (bind-true ($indexed-type (env-function-type-indexed-type $env $symbol $types))
      (cond
        ($indexed-type
          (lets
            ($type (indexed-value $indexed-type))
            ($index (indexed-index $indexed-type))
            ($var (variable $index))
            (typed 
              (application $var $terms)
              $type))))))

  (define (resolve-tuple-ref $symbol $types $terms)
    (and (= (length $types) 1)
      (bind ($type (car $types))
        (and (tuple-type? $type)
          (bind-true 
            ($indexed-type 
              (map-find-indexed
                (lambda ($field-type)
                  (and (type-named? $field-type $symbol) $field-type))
                (tuple-type-types $type)))
            (typed 
              (tuple-ref
                (typed (car $terms) $type)
                (indexed-index $indexed-type))
              (indexed-value $indexed-type)))))))

  (define (phase-tuple $phase $symbol $types $terms)
    (bind ($phase-depth (phase-depth $phase))
      (if (> $phase-depth 0)
        (typed
          (tuple-type $symbol $terms)
          (universe (- $phase-depth 1)))
        (typed
          (tuple $symbol (map typed $terms $types))
          (tuple-type $symbol $types)))))

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
    (lets
      ($typed (env-parse $env $phase $stx))
      ($type (typed-type $typed))
      (if (matches? $as-type $typed)
        (typed (typed-value $typed) $as-type)
        (syntax-error $stx
          (format "should be ~s, is ~s:" $as-type $type)))))

  (define (env-parse-proc $env $phase $stx)
    (lets
      ($typed (env-parse $env $phase $stx))
      ($type (typed-type $typed))
      (and (function-type? $type)
        (syntax-error $stx
          (format "should be procedure, is ~s:" $type)))))

  (define (env-parse-type $env $stx)
    (lets
      ($typed (env-parse $env (phase 1) $stx))
      ($value (typed-value $typed))
      ($type (typed-type $typed))
      (if (and (universe? $type) (= (universe-depth $type) 0))
        $value
        (syntax-error $stx 
          (format "should be universe 0:")))))

  (define (env-parse $env $phase $stx)
    (syntax-case $stx (native boolean number string function function-type use type select)
      ((native $value $type)
        (typed 
          (native (syntax->datum #`$value))
          (env-parse-type $env #`$type)))
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
        (lets
          ($name (syntax->datum #`name))
          ($params (syntax->list #`(param ...)))
          ($body #`body)
          ($param-types (map (partial env-parse-type $env) $params))
          ($arity (length $params))
          ($body-env (append (reverse $param-types) $env))
          ($typed-body (env-parse $body-env $phase $body))
          ($body-term (typed-value $typed-body))
          ($body-type (typed-type $typed-body))
          (typed
            (function $arity $body-term)
            (function-type $name $param-types $body-type))))
      ((use (expr ...) body)
        (lets
          ($exprs (syntax->list #`(expr ...)))
          ($body #`body)
          ($arity (length $exprs))
          ($typed-terms (env-parse-list $env $phase $exprs))
          ($terms (map typed-value $typed-terms))
          ($types (map typed-type $typed-terms))
          ($body-env (append (reverse $types) $env))
          ($typed-body (env-parse $body-env $phase $body))
          ($body-term (typed-value $typed-body))
          ($body-type (typed-type $typed-body))
          (typed
            (application (function $arity $body-term) $terms)
            $body-type)))
      ((select option ...)
        (lets
          ($options (map (partial env-parse-option $env $phase) (syntax->list #`(option ...))))
          ($size (length $options))
          ($types (map option-type $options))
          ($selection (options-selection $options))
          (switch $selection
            ((no-selection? _) (syntax-error $stx "no selection:"))
            ((multi-selection? _) (syntax-error $stx "multi selection:"))
            ((selected? $selected)
              (typed
                (select 
                  $size
                  (selected-index $selected)
                  (selected-term $selected))
                (choice-type $types)))
            ((else $other) (throw non-selection $other)))))
      ((id arg ...) (identifier? #`id)
        (lets
          ($symbol (syntax->datum #`id))
          ($args (syntax->list #`(arg ...)))
          ($typed-args (env-parse-list $env $phase $args))
          ($arg-types (map typed-type $typed-args))
          ($arg-terms (map typed-value $typed-args))
          ($type (tuple-type $symbol $arg-types))
          (or 
            (env-resolve-application $env $symbol $arg-types $arg-terms)
            (resolve-tuple-ref $symbol $arg-types $arg-terms)
            (phase-tuple $phase $symbol $arg-types $arg-terms))))
      (_
        (switch (syntax->datum $stx)
          ((boolean? $boolean) 
            (typed $boolean (boolean-type)))
          ((number? $number)
            (typed $number (number-type)))
          ((string? $string) 
            (typed $string (string-type)))
          ((symbol? $symbol)
            (lets 
              ($indexed-type (env-select-indexed-type $env $symbol))
              (if $indexed-type
                (typed 
                  (variable (indexed-index $indexed-type))
                  (indexed-value $indexed-type))
                (typed #f $symbol))))))))

  ; ---------------------------------------------------------

  (data (option-not type))
  (data (option-the typed))

  (define (option-type $option)
    (switch $option
      ((option-not? $option-not) (option-not-type $option-not))
      ((option-the? $option-the) (typed-type (option-the-typed $option-the)))))
  
  (define (env-parse-option $env $phase $stx)
    (syntax-case $stx (not)
      ((not expr)
        (option-not (env-parse-type $env #`expr)))
      (expr
        (option-the (env-parse $env $phase #`expr)))))

  ; ---------------------------------------------------------

  (data (no-selection))
  (data (selected index term))
  (data (multi-selection))

  (define (indexed-option-selected $indexed-option)
    (switch (indexed-value $indexed-option)
      ((option-the? $option-the)
        (selected
          (indexed-index $indexed-option)
          (typed-value (option-the-typed $option-the))))
      ((option-not? _) #f)
      ((else $other) (throw non-option $other))))

  (define (options-selection $options)
    (lets
      ($indexed-options (list-indexed $options))
      ($selections (filter (lambda (x) x) (map indexed-option-selected $indexed-options)))
      (case (length $selections)
        ((0) (no-selection))
        ((1) (car $selections))
        (else (multi-selection)))))
  
  ; --------------------------------------------------------

  (define-syntax evaluate!
    (syntax-rules ()
      ((_ expr)
        (evaluate (phase 0) #'expr))))

  (define (evaluate $phase $stx)
    (lets
      ($typed (env-parse (list) $phase $stx))
      ($term (typed-value $typed))
      ($type (typed-type $typed))
      (typed
        (eval-term $term
          (environment `(micascheme) `(term) `(type)))
        $type)))
)
