(library (leo compiler)
  (export
    compiler! compiler compiler? compiler-types
    compile)
  (import
    (except (micascheme) compile)
    (leo value))

  (data (compiler types))

  (define (compiler+type $compiler $type)
    (compiler
      (push (compiler-types $compiler) $type)))

  (define (compiler+types $compiler $types)
    (fold-left compiler+type $compiler $types))

  (define-syntax-rule (compiler! $type ...)
    (compiler (stack $type ...)))

  (define (compile $compiler $value)
    (switch $value
      ((typed? $typed) $typed)
      ((boolean? $boolean)
        (typed $boolean boolean!))
      ((number? $number)
        (typed $number number!))
      ((string? $string)
        (typed $string string!))
      ((named? $named)
        (lets
          ($compiled (compile $compiler (named-value $named)))
          (typed
            (typed-value $compiled)
            (named (named-name $named) (typed-type $compiled)))))
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
      ((variable? $variable)
        (lets
          ($types (compiler-types $compiler))
          ($type (variable-type $variable))
          ($index
            (find-index
              (lambda ($indexed-type) (obj=? $type $indexed-type))
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
              (for-all obj=? (map typed-type $compiled-args) (function-params $function-type))
              (throw illegal-arg-types)))
          (typed
            `(
              ,(typed-value $compiled-function)
              ,@(map typed-value $compiled-args))
            (function-body $function-type))))
      ((else $other) (throw compile $other))))
)
