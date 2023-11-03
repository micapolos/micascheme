(library (tico compiled)
  (export
    symbolic symbolic? symbolic-symbol symbolic-value
    packet packet? packet-comptime packet-runtime
    constant constant? constant-value
    variable variable? variable-index
    hole hole?
    compiled compiled? compiled-globals compiled-value

    globals comptime runtime

    pure-compiled
    compiled-with-value
    compiled+global
    compiled-bind
    compiled-lets
    compiled-flatten
    compiled-globalize

    packet-with-comptime

    type-literal->compiled
    boolean->compiled
    number->compiled
    string->compiled
    literal->compiled

    compiled-struct
    typed-struct
    packet-struct
    comptime-struct
    variable-struct

    symbolic-comptime
    typed-comptime
    compiled-comptime)
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

  (define-syntax-rule (globals $item ...) (stack $item ...))
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
    (compiled-lets
      ($typed-items (compiled-flatten $compiled-items))
      (pure-compiled
        (typed-struct
          (apply append (reverse (map compiled-globals $compiled-items)))
          $name
          $typed-items))))

  (define (typed-struct $globals $name $typed-items)
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
    `(lets
      ,@(reverse (map symbolic-comptime (compiled-globals $compiled)))
      ,(typed-comptime (compiled-value $compiled))))

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
)
