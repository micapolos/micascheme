(library (compiler)
  (export 
    compile! leo-compile
    
    ; aux keywords
    boolean number type select nth first second tuple get choice any)

  (import
    (except (micascheme) pair)
    (term)
    (type)
    (typed))

  (define-aux-keyword boolean)
  (define-aux-keyword number)
  (define-aux-keyword type)
  (define-aux-keyword select)
  (define-aux-keyword nth)
  (define-aux-keyword first)
  (define-aux-keyword second)
  (define-aux-keyword tuple)
  (define-aux-keyword get)
  (define-aux-keyword choice)
  (define-aux-keyword any)

  ; ----------------------------------------------------------------

  (data (frame types))
  (data (scope frames))
  (data (compiler scope parent-opt))

  (define-syntax-rule (frame! type ...)
    (frame (list type ...)))

  (define-syntax-rule (scope! frame ...)
    (scope (list frame ...)))

  ; ----------------------------------------------------------------

  (define (frame-size $frame)
    (length (frame-types $frame)))

  (define (frame-push-type $frame $type)
    (frame (cons $type (frame-types $frame))))

  (define (frame-typed-variables-from $frame $index)
    (map-indexed
      (lambda ($type-index $type) 
        (typed (variable (+ $type-index $index)) $type))
      (frame-types $frame)))

  (define (frame-resolve-all-from $frame $resolve $index)
    (filter $resolve (frame-typed-variables-from $frame $index)))

  ; ----------------------------------------------------------------

  (define (scope-push-frame $scope $frame)
    (scope (cons $frame (scope-frames $scope))))

  (define (scope-resolve-all-from $scope $resolve $index)
    (switch (scope-frames $scope)
      ((null? $null) `())
      ((else $frame-pair)
        (lets 
          ($frame (car $frame-pair))
          ($frames (cdr $frame-pair))
          ($resolved (frame-resolve-all-from $frame $resolve $index))
          (cond
            ((null? $resolved) (scope-resolve-all-from (scope $frames) $resolve (+ $index (frame-size $frame))))
            (else $resolved))))))

  ; ----------------------------------------------------------------

  (define null-compiler (compiler (scope!) #f))

  (define (compiler-parent $compiler)
    (or (compiler-parent-opt $compiler) null-compiler))

  (define (compiler-push-frame $compiler $frame)
    (compiler 
      (scope-push-frame (compiler-scope $compiler) $frame)
      (compiler-parent-opt $compiler)))

  ; ---------------------------------------------------------------

  (data (env types))

  (define null-env (env `()))

  (define (env-plus-type $env $type)
    (env (cons $type (env-types $env))))

  (define (env-type-ref $env $index)
    (list-ref (env-types $env) $index))

  ; ----------------------------------------------------------------

  (data (phase depth))

  (define (phase-depth=? $phase $depth)
    (= (phase-depth $phase) $depth))

  (define (next-phase $phase)
    (phase (+ (phase-depth $phase) 1)))

  ; ----------------------------------------------------------------

  (define (env-select-indexed-type $env $symbol)
    (map-find-indexed 
      (lambda ($type) (and (type-named? $type $symbol) $type))
      (env-types $env)))

  (define (env-function-type-indexed-type $env $symbol $arg-types)
    (map-find-indexed 
      (lambda ($type) 
        (and
          (function-type? $type)
          (symbol=? (function-type-name $type) $symbol)
          (list-matches? (function-type-params $type) $arg-types)
          (function-type-result $type)))
      (env-types $env)))

  ; ----------------------------------------------------------------

  (define (frame-symbol->indexed-types $frame $symbol)
    (lets
      ($indexed-types
        (map-indexed
          (lambda ($index $type) (indexed $type $index))
          (frame-types $frame)))
      (filter
        (lambda ($indexed-type)
          (eq? (type-selector (indexed-value $indexed-type)) $symbol))
        $indexed-types)))

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
    (opt-lets
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
          (opt-lets
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
    (env-compile null-env (phase 0) $stx))

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

  (define (env-compile-bind $env $phase $stx $fn)
    (lets
      ($arg (env-compile $env $phase $stx))
      ($arg-type (typed-type $arg))
      ($typed-body ($fn (env-plus-type $env $arg-type)))
      (typed
        (application!
          (function 1 (typed-value $typed-body))
          (typed-value $arg))
        (typed-type $typed-body))))

  (define (env-compile-bind* $env $phase $stxs $fn)
    (cond
      ((null? $stxs) ($fn $env))
      (else 
        (env-compile-bind $env $phase (car $stxs)
          (lambda ($env) 
            (env-compile-bind* $env $phase (cdr $stxs) $fn))))))
    
  (define (env-compile $env $phase $stx)
    (syntax-case $stx (any get tuple apply nth variable first choice second lets native boolean number string if recursive function function-type use type select switch)
      ((native $value $type)
        (typed 
          (native #`$value)
          (env-compile-type $env #`$type)))
      ((type expr)
        (typed (env-compile-type $env #`expr) type!))
      (boolean (phase-depth=? $phase 1)
        (typed boolean! type!))
      (number (phase-depth=? $phase 1)
        (typed number! type!))
      (string (phase-depth=? $phase 1)
        (typed string! type!))
      (type (phase-depth=? $phase 1)
        (typed type! type!))
      ((any (function (name param ...) rhs))
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
          ($env (fold-left env-plus-type $env $param-types))
          ($typed-body (env-compile $env $phase $body))
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
      ((recursive $type $body)
        (lets
          ($type (env-compile-type $env #`$type))
          ($typed (env-compile-as (env-plus-type $env $type) $phase #`$body $type))
          (typed (recursive (typed-value $typed)) $type)))
      ((tuple name arg ...)
        (typed-tuple
          (syntax->datum #`name)
          (env-compile-list $env $phase (syntax->list #`(arg ...)))))
      ((get lhs selector)
        (lets
          ($typed (env-compile $env $phase #`lhs))
          (switch (typed-type $typed)
            ((tuple-type? $tuple-type)
              (typed-tuple-ref $typed (syntax->datum #`selector)))
            ((else $other) (syntax-error $stx)))))
      ((use expr ... body)
        (lets
          ($exprs (syntax->list #`(expr ...)))
          ($body #`body)
          ($arity (length $exprs))
          ($typed-terms (env-compile-list $env $phase $exprs))
          ($terms (map typed-value $typed-terms))
          ($types (map typed-type $typed-terms))
          ($env (fold-left env-plus-type $env $types))
          ($typed-body (env-compile $env $phase $body))
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
          (_ (unless (number? $num) (syntax-error #`num "should be number")))
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
      ((switch $choice case ...)
        (lets
          ($choice #`$choice)
          ($typed-choice (env-compile $env $phase $choice))
          ($choice-type (typed-type $typed-choice))
          ($choice-term (typed-value $typed-choice))
          ($choice-types (choice-type-types $choice-type))
          ($cases (syntax->list #`(case ...)))
          ($head-typed-case (env-compile (env-plus-type $env (car $choice-types)) $phase (car $cases)))
          ($head-type (typed-type $head-typed-case))
          ($tail-typed-cases
            (map
              (lambda ($case $type) (env-compile-as (env-plus-type $env $type) $phase $case $head-type))
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
      ((lets arg ... expr)
        (env-compile-bind* $env $phase (syntax->list #`(arg ...))
          (lambda ($env)
            (env-compile $env $phase #`expr))))
      ((variable index)
        (lets
          ($index (syntax->datum #`index))
          (typed (variable $index) (env-type-ref $env $index))))
      ((choice arg ...)
        (typed
          (choice-type (map typed-value (env-compile-list $env $phase (syntax->list #`(arg ...)))))
          type!))
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
                (case $symbol
                  ((boolean) (typed boolean! type!))
                  ((number) (typed number! type!))
                  ((string) (typed string! type!))
                  ((type) (typed type! (universe 1)))
                  (else (typed #f $symbol))))))))))

  (define (env-compile-option $env $phase $stx)
    (syntax-case $stx (not)
      ((not expr)
        (option-not (env-compile-type $env #`expr)))
      (expr
        (option-the (env-compile $env $phase #`expr)))))

  ; =====================================================================

  (define typed-env
    (list
      (typed 
        (native #`string-append) 
        (function-type! (append string! string!) string!))
      (typed 
        (native #`string-length)
        (function-type! (length string!) number!))
      (typed 
        (native #`number->string)
        (function-type! (string number!) number!))
      (typed 
        (native #`+)
        (function-type! (+ number! number!) number!))
      (typed 
        (native #`-)
        (function-type! (- number! number!) number!))

      ; pair
      (typed 
        (native #`cons)
        (function 2 (function-type! (pair (variable 1) (variable 0)) (pair-type (variable 1) (variable 0)))))
      (typed 
        (native #`car)
        (function 2 (function-type! (first (pair (variable 1) (variable 0))) (variable 1))))
      (typed 
        (native #`cdr)
        (function 2 (function-type! (second (pair (variable 1) (variable 0))) (variable 0))))

      ; list
      (typed 
        (native #`()) 
        (function 1 (function-type! (list-of (variable 0)) (list-of (variable 0)))))
      (typed 
        (native #`cons)
        (function 1 (function-type! (link (variable 0) (list (variable 0))) (list-of (variable 0)))))))
)

