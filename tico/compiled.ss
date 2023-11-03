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

    compiled-pure compiled-bind compiled-lets

    type-literal->compiled
    boolean->compiled
    number->compiled
    string->compiled
    literal->compiled)
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
  (define-syntax-rule (comptime $item) $item)
  (define-syntax-rule (runtime $item) $item)

  (define (compiled-pure $value)
    (compiled (globals) $value))

  (define (compiled-bind $compiled $fn)
    (lets
      ($globals (compiled-globals $compiled))
      ($fn-compiled ($fn (compiled-value $compiled)))
      (compiled
        (push-all $globals (compiled-globals $fn-compiled))
        (compiled-value $fn-compiled))))

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
)
