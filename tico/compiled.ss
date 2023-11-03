(library (tico compiled)
  (export
    symbolic symbolic? symbolic-symbol symbolic-value
    packet packet? packet-comptime packet-runtime
    constant constant? constant-value
    variable variable? variable-index
    hole hole?
    compiled compiled? compiled-globals compiled-value

    globals locals comptime runtime

    pure-compiled
    compiled-with-value
    compiled+global
    compiled-bind
    compiled-lets
    compiled-flatten
    compiled-globalize

    compiled-map-constant
    typed-map-constant
    packet-map-constant
    runtime-map-constant
    constant-map

    packet-with-comptime

    type-literal->compiled
    boolean->compiled
    number->compiled
    string->compiled
    literal->compiled
    literal-typed
    literal-packet

    compiled-lambda
    typed-lambda
    packet-lambda

    locals->typed-variable-opt
    locals->typed-variable

    compiled-struct
    typed-struct
    packet-struct
    comptime-struct
    runtime-struct
    variable-struct

    comptime->runtime
    symbolic-comptime
    typed-comptime
    compiled-comptime

    typed-local)
  (import
    (micascheme)
    (tico type)
    (tico typed)
    (tico expression)
    (evaluator))

  (data (symbolic symbol value))
  (data (packet comptime runtime))
  (data (constant value))
  (data (variable index))
  (data (hole))
  (data (compiled globals value))

  ;(alias (global (symbolic symbol (packet datum value))))
  ;(alias (local (typed (type) (packet symbol (oneof (constant value) hole)))))

  (define-syntax-rule (globals $item ...) (stack $item ...))
  (define-syntax-rule (locals $item ...) (stack $item ...))
  (define-syntax-rule (global $item) $item)
  (define-syntax-rule (comptime $item) $item)
  (define-syntax-rule (runtime $item) $item)

  (define (pure-compiled $value)
    (compiled (globals) $value))

  (define (compiled-with-value $compiled $value)
    (compiled
      (compiled-globals $compiled)
      $value))

  (define (compiled+global $compiled $global)
    (compiled
      (push
        (compiled-globals $compiled)
        $global)
      (compiled-value $compiled)))

  (define (compiled-bind $compiled $fn)
    (lets
      ($globals (compiled-globals $compiled))
      ($fn-compiled ($fn (compiled-value $compiled)))
      (compiled
        (push-all $globals (compiled-globals $fn-compiled))
        (compiled-value $fn-compiled))))

  (define (compiled-flatten $list)
    (if (null? $list)
      (pure-compiled (list))
      (compiled-lets
        ($car (car $list))
        ($cdr (compiled-flatten (cdr $list)))
        (pure-compiled (cons $car $cdr)))))

  (define (compiled-globalize $compiled)
    (lets
      ($typed (compiled-value $compiled))
      ($packet (typed-value $typed))
      (switch (packet-runtime $packet)
        ((constant? $constant)
          (lets
            ($symbol (generate-symbol))
            (compiled+global
              (compiled-with-value $compiled
                (typed-with-value $typed
                  (packet-with-comptime $packet (comptime $symbol))))
              (global
                (symbolic $symbol
                  (packet-with-runtime $packet
                    (constant-value $constant)))))))
        ((variable? $variable) $compiled))))
  (define (packet-with-comptime $packet $comptime)
    (packet $comptime (packet-runtime $packet)))

  (define (packet-with-runtime $packet $runtime)
    (packet (packet-comptime $packet) $runtime))

  (define-syntax compiled-lets
    (syntax-rules ()
      ((_ $body) $body)
      ((_ ($value $compiled) $decl ... $body)
        (identifier? #'$value)
        (compiled-bind $compiled
          (lambda ($value)
            (compiled-lets $decl ... $body))))))

  (define (compiled-map-constant $compiled $fn)
    (compiled-with-value $compiled
      (typed-map-constant (compiled-value $compiled) $fn)))

  (define (typed-map-constant $typed $fn)
    (typed-with-value $typed
      (and (typed-value $typed)
        (packet-map-constant (typed-value $typed) $fn))))

  (define (packet-map-constant $packet $fn)
    (packet-with-runtime $packet
      (runtime-map-constant (packet-runtime $packet) $fn)))

  (define (runtime-map-constant $runtime $fn)
    (switch $runtime
      ((constant? $constant) (constant-map $constant $fn))
      ((variable? $variable) $variable)))

  (define (constant-map $constant $fn)
    (constant ($fn (constant-value $constant))))

  (define (literal-typed $literal)
    (typed
      (literal-type $literal)
      (literal-packet $literal)))

  (define (literal-packet $literal)
    (packet
      (comptime $literal)
      (runtime (constant $literal))))

  (define (type-literal->compiled $type $literal)
    (pure-compiled
      (typed
        $type
        (packet
          (comptime $literal)
          (runtime (constant $literal))))))

  (define (boolean->compiled $boolean)
    (type-literal->compiled (boolean-type) $boolean))

  (define (number->compiled $number)
    (type-literal->compiled (number-type) $number))

  (define (string->compiled $string)
    (type-literal->compiled (string-type) $string))

  (define (literal->compiled $literal)
    (switch $literal
      ((boolean? $boolean)
        (boolean->compiled $literal))
      ((number? $number)
        (number->compiled $number))
      ((string? $string)
        (string->compiled $string))
      ((else $other)
        (throw not-literal $other))))

  (define (compiled-struct $name $compiled-items)
    (lets
      ($items-compiled (compiled-flatten $compiled-items))
      ($globals (compiled-globals $items-compiled))
      (compiled-lets
        ($typed-items $items-compiled)
        (pure-compiled
          (typed-struct $name $globals $typed-items)))))

  (define (typed-struct $name $globals $typed-items)
    (lets
      ($types (map typed-type $typed-items))
      ($packets (typed-list->dynamic-values $typed-items))
      (typed
        (struct $name $types)
        (packet-struct $globals $packets))))

  (define (packet-struct $globals $packets)
    (lets
      ($comptimes (map packet-comptime $packets))
      ($runtimes (map packet-runtime $packets))
      ($comptime (comptime-struct $comptimes))
      (packet $comptime
        (runtime-struct $globals $comptime $runtimes))))

  (define (comptime-struct $comptimes)
    (comptime (tuple-expression $comptimes)))

  (define (runtime-struct $globals $comptime $runtimes)
    (runtime
      (or
        (and
          (for-all constant? $runtimes)
          (constant (comptime->runtime $globals $comptime)))
        (variable-struct (filter variable? $runtimes)))))

  (define (variable-struct $variables)
    (unless (not (null? $variables)) (throw null? $variables))
    (variable
      (apply max
        (map variable-index $variables))))

  (define (symbolic-comptime $symbolic)
    `(
      ,(symbolic-symbol $symbolic)
      ,(packet-comptime (symbolic-value $symbolic))))

  (define (typed-comptime $typed)
    (packet-comptime (typed-value $typed)))

  (define (compiled-comptime $compiled)
    (lets
      ($comptime (typed-comptime (compiled-value $compiled)))
      (switch (compiled-globals $compiled)
        ((null? _) $comptime)
        ((else $globals)
          `(lets
            ,@(reverse (map symbolic-comptime $globals))
            ,$comptime)))))

  (define (comptime->runtime $globals $comptime)
    (evaluate
      (evaluator
        (environment `(micascheme))
        (map
          (lambda ($symbolic)
            (cons
              (symbolic-symbol $symbolic)
              (packet-runtime (symbolic-value $symbolic))))
          $globals))
      $comptime))

  (define (typed-local $typed)
    (lets
      ($type (typed-type $typed))
      (typed $type
        (and (type-dynamic? $type)
          (lets
            ($packet (typed-value $typed))
            ($symbol (generate-symbol))
            (packet
              (comptime $symbol)
              (runtime
                (switch (packet-runtime $packet)
                  ((constant? $constant) $constant)
                  ((variable? $variable) (hole))))))))))

  ; --- compiled-variable

  (define (locals->compiled-variable $locals $pattern)
    (pure-compiled
      (locals->typed-variable $locals $pattern)))

  (define (locals->typed-variable $locals $pattern)
    (or
      (locals->typed-variable-opt $locals $pattern)
      (throw variable-not-found $pattern)))

  (define (locals->typed-variable-opt $locals $pattern)
    (locals-index->typed-variable-opt $locals 0 $pattern))

  (define (locals-index->typed-variable-opt $locals $index $pattern)
    (and (pair? $locals)
      (or
        (local-index->typed-variable-opt
          (car $locals) $index $pattern)
        (locals-index->typed-variable-opt
          (cdr $locals) (+ $index 1) $pattern))))

  (define (local-index->typed-variable-opt $local $index $pattern)
    (lets
      ($type (typed-type $local))
      (and (type-matches? $type $pattern)
        (typed $type
          (and (type-dynamic? $type)
            (lets
              ($local-packet (typed-value $local))
              (packet
                (comptime (packet-comptime $local-packet))
                (runtime
                  (switch (packet-runtime $local-packet)
                    ((constant? $constant) $constant)
                    ((hole? $hole) (variable $index)))))))))))

  ; --- compiled-lambda

  (define (compiled-lambda $param-locals $compiled-body)
    (lets
      ($globals (compiled-globals $compiled-body))
      ($typed-body (compiled-value $compiled-body))
      (compiled-with-value $compiled-body
        (typed-lambda $globals $param-locals $typed-body))))

  (define (typed-lambda $globals $param-locals $typed-body)
    (lets
      ($body-type (typed-type $typed-body))
      ($param-types (map typed-type $param-locals))
      ($arrow (arrow (reverse $param-types) $body-type))
      (typed $arrow
        (and (type-dynamic? $arrow)
          (packet-lambda $globals
            (typed-list->dynamic-values $param-locals)
            (typed-value $typed-body))))))

  (define (packet-lambda $globals $param-packets $body-packet)
    (lets
      ($comptime
        (comptime-lambda
          (map packet-comptime $param-packets)
          (packet-comptime $body-packet)))
      (packet $comptime
        (runtime-lambda $globals $comptime
          (map packet-runtime $param-packets)
          (packet-runtime $body-packet)))))

  (define (comptime-lambda $params $body)
    `(lambda (,@(reverse $params)) ,$body))

  (define (runtime-lambda $globals $comptime $runtime-params $body-runtime)
    (switch $body-runtime
      ((constant? $constant)
        (constant (comptime->runtime $globals $comptime)))
      ((variable? $variable)
        (lets
          ($arity (length $runtime-params))
          ($index (- (variable-index $variable) $arity))
          (cond
            ((< $index 0)
              (constant (comptime->runtime $globals $comptime)))
            (else
              (variable $index)))))))

  ; --- compiled-application

  (define (compiled-application $compiled-target $compiled-args)
    (lets
      ($compiled-proc
        (compiled-lets
          ($typed-target $compiled-target)
          ($typed-args (compiled-flatten $compiled-args))
          (pure-compiled
            (lambda ($globals)
              (typed-application $globals $typed-target $typed-args)))))
      (app
        (compiled-value $compiled-proc)
        (compiled-globals $compiled-proc))))

  (define (typed-application $globals $typed-target $typed-args)
    (lets
      ($target-type (typed-type $typed-target))
      ($arg-types (map typed-type $typed-args))
      (switch $target-type
        ((arrow? $arrow)
          (typed
            (arrow-result $arrow)
            (and (type-dynamic? $arrow)
              (packet-application $globals
                (typed-value $typed-target)
                (typed-list->dynamic-values $typed-args)))))
        ((else $other)
          (throw not-arrow $target-type)))))

  (define (packet-application $globals $packet-target $packet-args)
    TODO)
)
