(library (leo compiler)
  (export
    compiler! compiler compiler? compiler-types compiler-tmp-count
    compile)
  (import
    (except (micascheme) compile)
    (leo value))

  (data (compiler types tmp-count))

  (define (compiler+type $compiler $type)
    (compiler
      (push (compiler-types $compiler) $type)
      (compiler-tmp-count $compiler)))

  (define (compiler+tmp $compiler)
    (compiler
      (compiler-types $compiler)
      (+ (compiler-tmp-count $compiler) 1)))

  (define (compiler+types $compiler $types)
    (fold-left compiler+type $compiler $types))

  (define-syntax-rule (compiler! $type ...)
    (compiler (stack $type ...) 0))

  (define (compile $compiler $value)
    (switch $value
      ((typed? $typed) $typed)
      ((boolean? $boolean)
        (typed $boolean boolean!))
      ((number? $number)
        (typed $number number!))
      ((string? $string)
        (typed $string string!))
      ((typeof? $typeof)
        (lets
          ($value (typeof-value $typeof))
          ($compiled (compile $compiler $value))
          ($type (typed-type $compiled))
          (typed $type type!)))
      ((named? $named)
        (lets
          ($compiled (compile $compiler (named-value $named)))
          (typed
            (typed-value $compiled)
            (named (named-name $named) (typed-type $compiled)))))
      ((named-get? $named-get)
        (lets
          ($named (named-get-named $named-get))
          ($compiled (compile $compiler $named))
          ($value (typed-value $compiled))
          ($type (typed-type $compiled))
          (do (unless (named? $type) (throw not-named $named-get)))
          (typed $value (named-value $type))))
      ((tuple? $tuple)
        (lets
          ($items (tuple-items $tuple))
          ($compiled-items (map (partial compile $compiler) $items))
          ($values (map typed-value $compiled-items))
          ($types (map typed-type $compiled-items))
          (typed
            (case (length $items)
              ((0) `(void))
              ((1) (car $values))
              ((2) `(cons ,(car $values) ,(cadr $values)))
              (else `(vector ,@$values)))
            (tuple $types))))
      ((tuple-get? $tuple-get)
        (lets
          ($tuple (tuple-get-tuple $tuple-get))
          ($type (tuple-get-type $tuple-get))
          ($compiled-tuple (compile $compiler $tuple))
          ($value (typed-value $compiled-tuple))
          ($tuple-type (typed-type $compiled-tuple))
          (do (unless (tuple? $tuple-type) (throw not-tuple $tuple)))
          ($types (tuple-items $tuple-type))
          ($index
            (find-index
              (lambda ($indexed-type) (equal? $type $indexed-type))
              $types))
          (do (unless $index (throw tuple-get-type-not-found $type)))
          (typed
            (case (length $types)
              ((0) (throw impossible))
              ((1) $value)
              ((2) `(,(if (zero? $index) `car `cdr) ,$value))
              (else `(vector-ref ,$value ,$index)))
            $type)))
      ((choice-switch? $choice-switch)
        (lets
          ($choice (choice-switch-choice $choice-switch))
          ($cases (choice-switch-cases $choice-switch))
          (do (when (null? $cases) (throw no-cases $choice-switch)))
          ($compiled-choice (compile $compiler $choice))
          ($value (typed-value $compiled-choice))
          ($type (typed-type $compiled-choice))
          (do (unless (choice? $type) (throw not-choice $choice)))
          ($choice-types (choice-items $type))
          (do
            (unless
              (= (length $cases) (length $choice-types))
              (throw choice-switch-cases-mismatch $choice-switch)))
          ($case-compilers (map (partial compiler+type $compiler) $choice-types))
          ($compiled-cases (map compile $case-compilers $cases))
          ($case-values (map typed-value $compiled-cases))
          ($case-types (map typed-type $compiled-cases))
          ($result-type (car $case-types))
          (do
            (unless
              (for-all (lambda ($case-type) (equal? $case-type $result-type)) $case-types)
              (throw incompatible-case-types $choice-switch $case-types)))
          ($variable (typed-value (compile (car $case-compilers) (variable (car $choice-types)))))
          ($length (length $cases))
          ($last-index (- $length 1))
          (typed
            (case (length $cases)
              ((1)
                `(lets
                  (,$variable ,$value)
                  ,(car $case-values)))
              ((2)
                `(lets
                  ($tmp ,$value)
                  (,$variable (cdr $tmp))
                  (if (car $tmp)
                    ,(car $case-values)
                    ,(cadr $case-values))))
              (else
                `(lets
                  ($tmp ,$value)
                  (,$variable (cdr $tmp))
                  (case (car $tmp)
                    ,@(map-indexed
                      (lambda ($index $value)
                        `(
                          ,(if (= $index $last-index) `else `(,$index))
                          ,$value))
                      $case-values)))))
            $result-type)))
      ((variable? $variable)
        (lets
          ($types (compiler-types $compiler))
          ($type (variable-type $variable))
          ($index
            (find-index
              (lambda ($indexed-type) (equal? $type $indexed-type))
              $types))
          (if (not $index)
            (throw not-found $variable)
            (typed
              (string->symbol
                (string-append "v"
                  (number->string
                    (- (length $types) $index 1))))
              $type))))
      ((function? $function)
        (lets
          ($params (function-params $function))
          ($body (function-body $function))
          ($compiler (compiler+types $compiler $params))
          ($variables (map variable $params))
          ($compiled-variables (map (partial compile $compiler) $variables))
          ($compiled-body (compile $compiler $body))
          (typed
            `(lambda (,@(map typed-value $compiled-variables))
              ,(typed-value $compiled-body))
            (function $params (typed-type $compiled-body)))))
      ((application? $application)
        (lets
          ($function (application-function $application))
          ($args (application-args $application))
          ($compiled-function (compile $compiler $function))
          ($compiled-args (map (partial compile $compiler) $args))
          ($function-type (typed-type $compiled-function))
          (do
            (unless
              (function? $function-type)
              (throw not-function $function-type)))
          (do
            (unless
              (for-all equal? (map typed-type $compiled-args) (function-params $function-type))
              (throw illegal-arg-types)))
          (typed
            `(
              ,(typed-value $compiled-function)
              ,@(map typed-value $compiled-args))
            (function-body $function-type))))
      ((else $other)
        (throw compile $other))))
)
