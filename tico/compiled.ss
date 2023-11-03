(library (tico compiled)
  (export
    symbolic symbolic? symbolic-symbol symbolic-value
    typed typed? typed-type typed-value
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
    compiled-globalize

    packet-with-comptime
    typed-with-value

    type-literal->compiled
    boolean->compiled
    number->compiled
    string->compiled
    literal->compiled

    symbolic-comptime
    typed-comptime
    compiled-comptime)
  (import
    (micascheme)
    (tico type))

  (data (symbolic symbol value))
  (data (typed type value))
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

  (define (typed-with-value $typed $value)
    (typed (typed-type $typed) $value))

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
    (compiled
      (globals)
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
)
