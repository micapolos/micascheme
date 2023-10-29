(library (tico-3)
  (export)
  (import (micascheme))

  (data (constant value))
  (data (variable index))
  (data (hole))
  (data (thunk runtime bindings datum))

  (define (parse $bindings $datum)
    (syntax-case $datum ()
      (($lambda ($param ...) $body)
        (identifier-named? (syntax $lambda) lambda)
        (todo))
      (($fn $arg ...)
        (lets
          ($arg-thunks (map (partial parse $bindings) (syntax->list (syntax $arg ...))))
          ($params (generate-symbols (length $arg-thunks)))
          (thunk
            )
      ($identifier
        (identifier? (syntax $identifier))
        (lets
          ($symbol (syntax->datum (syntax $identifier)))
          ($found
            (map-find-indexed
              (lambda ($binding)
                (and
                  (symbol=? $symbol (car $binding))
                  (cdr $binding)))
              $bindings))
          (switch $found
            ((indexed? $indexed)
              (switch (indexed-value $indexed)
                ((constant? $constant)
                  ())
                ((hole? _)
                  (thunk
                    (free (indexed-index $index))

                    ))))
            (else
              (syntax-error (syntax $identifier) "unbound"))))))
      ($other
        (switch (syntax->datum (syntax $other))
          ((boolean $boolean)
            (thunk (constant $boolean $boolean) (stack) $boolean))
          ((number? $number)
            (thunk (constant $number $number) (stack) $number))
          ((string? $string)
            (thunk (constant $string $string) (stack) $string))
          ((else _)
            (syntax-error (syntax $other)))))))
)
