(library (tico compiled)
  (export
    constant constant? constant-value
    compiled compiled? compiled-type compiled-term compiled-free-variable-count compiled-constant-opt

    value-type-compiled
    boolean-compiled
    number-compiled
    string-compiled
    compiled-application
    )
  (import (micascheme) (tico term) (tico type))

  (data (constant value))
  (data (compiled type term free-variable-count constant-opt))

  (define (value-type-compiled $value $type)
    (compiled $type $value 0 (constant $value)))

  (define (boolean-compiled $boolean)
    (value-type-compiled $boolean (boolean-type)))

  (define (number-compiled $number)
    (value-type-compiled $number (number-type)))

  (define (string-compiled $string)
    (value-type-compiled $string (string-type)))

  (define (compiled-application $target $args)
    (compiled
      (switch (compiled-type $target)
        ((function-type? $function-type)
          (or
            (and
              (for-all type-matches?
                (map compiled-type $args)
                (function-type-params $function-type))
              (function-type-body $function-type))
            (error `compiled-application "args type mismatch")))
        ((else $other)
          (error `compiled-application "not function" $other)))
      (application
        (compiled-term $target)
        (map compiled-term $args))
      (max
        (compiled-free-variable-count $target)
        (apply max (map compiled-free-variable-count $args)))
      (lets
        ($target-constant-opt (compiled-constant-opt $target))
        ($arg-constant-opts (map compiled-constant-opt $args))
        (and
          $target-constant-opt
          (for-all identity $arg-constant-opts)
          (constant
            (apply
              (constant-value $target-constant-opt)
              (map constant-value $arg-constant-opts)))))))
)
