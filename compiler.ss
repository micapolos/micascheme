(library (compiler)
  (export 
    compile! leo-compile
    
    ; aux keywords
    boolean number type select nth first second)

  (import (micascheme) (term) (type) (typed))

  (define-aux-keyword boolean)
  (define-aux-keyword number)
  (define-aux-keyword type)
  (define-aux-keyword select)
  (define-aux-keyword nth)
  (define-aux-keyword first)
  (define-aux-keyword second)

  ; ----------------------------------------------------------------

  (data (compiler frames parent))
  (data (frame types))

  (define null-compiler (compiler `() #f))

  (define (compiler-update-parent $compiler $fn)
    (compiler
      (compiler-frames $compiler)
      ($fn (or (compiler-parent $compiler) null-compiler))))

  (define (compiler-push-frame $compiler $frame)
    (compiler 
      (cons $frame (compiler-frames $compiler))
      (compiler-parent $compiler)))

  ; ----------------------------------------------------------------

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
    (and-lets 
      ($indexed-type (env-function-type-indexed-type $env $symbol $types))
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
      (lets 
        ($term (car $terms))
        ($type (car $types))
        (and (tuple-type? $type)
          (and-lets 
            ($indexed-type 
              (map-find-indexed
                (lambda ($field-type)
                  (and (type-named? $field-type $symbol) $field-type))
                (tuple-type-types $type)))
            (lets 
              ($size (length (tuple-type-types $type)))
              ($index (indexed-index $indexed-type))
              ($type (indexed-value $indexed-type))
              (typed 
                (case $size
                  ((1) $term)
                  ((2) ((if (= $index 0) pair-first pair-second) $term))
                  (else (vector-get $term $index)))
                (indexed-value $indexed-type))))))))

  (define (phase-tuple $phase $symbol $types $terms)
    (lets
      ($phase-depth (phase-depth $phase))
      ($size (length $types))
      (if (= $phase-depth 0)
        (typed-tuple $symbol (map typed $terms $types))
        (typed
          (tuple-type $symbol $terms)
          (universe (- $phase-depth 1))))))

  ; ----------------------------------------------------------------

  (define-syntax compile!
    (syntax-rules ()
      ((_ expr)
        (leo-compile #'expr))))

  (define (leo-compile $stx)
    (env-compile (list) (phase 0) $stx))

  (define (v $index)
    (string->symbol
      (string-append "v"
        (number->string $index))))

  (define (env-compile-list $env $phase $stxs)
    (map (partial env-compile $env $phase) $stxs))

  (define (env-compile-as $env $phase $stx $as-type)
    (lets
      ($typed (env-compile $env $phase $stx))
      ($type (typed-type $typed))
      (if (matches? $as-type $type)
        (typed (typed-value $typed) $as-type)
        (syntax-error $stx
          (format "should be ~s, is ~s:" $as-type $type)))))

  (define (env-compile-function $env $phase $stx)
    (lets
      ($typed (env-compile $env $phase $stx))
      ($term (typed-value $typed))
      ($type (typed-type $typed))
      (if (function-type? $type)
        (typed $term $type)
        (syntax-error $stx
          (format "should be function, is ~s:" $type)))))

  (define (env-compile-type $env $stx)
    (lets
      ($typed (env-compile $env (phase 1) $stx))
      ($value (typed-value $typed))
      ($type (typed-type $typed))
      (if (and (universe? $type) (= (universe-depth $type) 0))
        $value
        (syntax-error $stx 
          (format "should be universe 0:")))))

  (define (env-compile $env $phase $stx)
    (syntax-case $stx (apply nth first second native boolean number string if recursive function function-type use type select switch)
      ((native $value $type)
        (typed 
          (native #`$value)
          (env-compile-type $env #`$type)))
      ((type expr)
        (typed (env-compile-type $env #`expr) type!))
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
            (map (partial env-compile-type $env) (syntax->list #`(param ...)))
            (env-compile-type $env #`rhs))
          type!))
      ((function (name param ...) body)
        (lets
          ($name (syntax->datum #`name))
          ($params (syntax->list #`(param ...)))
          ($body #`body)
          ($param-types (map (partial env-compile-type $env) $params))
          ($arity (length $params))
          ($body-env (append (reverse $param-types) $env))
          ($typed-body (env-compile $body-env $phase $body))
          ($body-term (typed-value $typed-body))
          ($body-type (typed-type $typed-body))
          (typed
            (function $arity $body-term)
            (function-type $name $param-types $body-type))))
      ((apply fn arg ...)
        (lets
          ($function #`fn)
          ($typed-function (env-compile-function $env $phase $function))
          ($function (typed-value $typed-function))
          ($function-type (typed-type $typed-function))
          ($params (function-type-params $function-type))
          ($args (syntax->list #`(arg ...)))
          ($typed-args (map (partial env-compile-as $env $phase) $args $params))
          (typed
            (application $function (map typed-value $typed-args))
            (function-type-result $function-type))))
      ((recursive result (function (name param ...) body))
        (lets
          ($result #`result)
          ($name (syntax->datum #`name))
          ($params (syntax->list #`(param ...)))
          ($body #`body)
          ($result-type (env-compile-type $env $result))
          ($param-types (map (partial env-compile-type $env) $params))
          ($arity (length $params))
          ($function-type (function-type $name $param-types $result-type))
          ($body-env (append (reverse $param-types) (cons $function-type $env)))
          ($typed-body (env-compile $body-env $phase $body))
          ($body-term (typed-value $typed-body))
          ($body-type (typed-type $typed-body))
          ($unused (unless (matches? $result-type $body-type) (syntax-error $stx "recursive type mismatch")))
          (typed
            (recursive (function $arity $body-term))
            (function-type $name $param-types $body-type))))
      ((use expr ... body)
        (lets
          ($exprs (syntax->list #`(expr ...)))
          ($body #`body)
          ($arity (length $exprs))
          ($typed-terms (env-compile-list $env $phase $exprs))
          ($terms (map typed-value $typed-terms))
          ($types (map typed-type $typed-terms))
          ($body-env (append (reverse $types) $env))
          ($typed-body (env-compile $body-env $phase $body))
          ($body-term (typed-value $typed-body))
          ($body-type (typed-type $typed-body))
          (typed
            (application (function $arity $body-term) $terms)
            $body-type)))
      ((if condition consequent alternate)
        (lets
          ($condition (env-compile-as $env $phase #`condition boolean!))
          ($consequent (env-compile $env $phase #`consequent))
          ($type (typed-type $consequent))
          ($alternate (env-compile-as $env $phase #`alternate $type))
          (typed
            (conditional 
              (typed-value $condition)
              (typed-value $consequent)
              (typed-value $alternate))
            $type)))
      ((nth num name expr)
        (lets
          ($num (syntax->datum #`num))
          ($unused (unless (number? $num) (syntax-error #`num "should be number")))
          "TODO"))
      ((first name expr)
        "TODO")
      ((second name expr)
        "TODO")
      ((select option ...)
        (lets
          ($options (map (partial env-compile-option $env $phase) (syntax->list #`(option ...))))
          ($size (length $options))
          ($types (map option-type $options))
          ($selection (options-selection $options))
          (switch $selection
            ((no-selection? _) (syntax-error $stx "no selection:"))
            ((multi-selection? _) (syntax-error $stx "multi selection:"))
            ((selected? $selected)
              (typed
                (pair (selected-index $selected) (selected-term $selected))
                (choice-type $types)))
            ((else $other) (throw non-selection $other)))))
      ((switch choice case ...)
        (lets
          ($choice #`choice)
          ($typed-choice (env-compile $env $phase $choice))
          ($choice-type (typed-type $typed-choice))
          ($choice-term (typed-value $typed-choice))
          ($choice-types (choice-type-types $choice-type))
          ($cases (syntax->list #`(case ...)))
          ($head-typed-case (env-compile (cons (car $choice-types) $env) $phase (car $cases)))
          ($head-type (typed-type $head-typed-case))
          ($tail-typed-cases
            (map
              (lambda ($case $type) (env-compile-as (cons $type $env) $phase $case $head-type))
              (cdr $cases)
              (cdr $choice-types)))
          ($typed-cases (cons $head-typed-case $tail-typed-cases))
          ($case-types (map typed-type $typed-cases))
          ($type (car $case-types))
          ($case-terms (map typed-value $typed-cases))
          (typed
            (use! $choice-term
              (use! (pair-second v0)
                (branch (pair-first v1) $case-terms)))
            $type)))
      ((id arg ...) (identifier? #`id)
        (lets
          ($symbol (syntax->datum #`id))
          ($args (syntax->list #`(arg ...)))
          ($typed-args (env-compile-list $env $phase $args))
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
            (typed $boolean boolean!))
          ((number? $number)
            (typed $number number!))
          ((string? $string) 
            (typed $string string!))
          ((symbol? $symbol)
            (lets 
              ($indexed-type (env-select-indexed-type $env $symbol))
              (if $indexed-type
                (typed 
                  (variable (indexed-index $indexed-type))
                  (indexed-value $indexed-type))
                (typed #f $symbol))))))))

  (define (env-compile-option $env $phase $stx)
    (syntax-case $stx (not)
      ((not expr)
        (option-not (env-compile-type $env #`expr)))
      (expr
        (option-the (env-compile $env $phase #`expr)))))
)

