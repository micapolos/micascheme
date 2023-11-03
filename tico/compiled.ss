(library (tico compiled)
  (export)
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
