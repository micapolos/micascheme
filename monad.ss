(library (monad)
  (export
    monad monad? monad-return-fn monad-bind-fn
    monad-return monad-bind monad-map monad-sequence monad-lift
    option-monad)
  (import (micascheme))

  (data (monad return-fn bind-fn))

  (define (monad-return $monad $value)
    ((monad-return-fn $monad) $value))

  (define (monad-bind $monad $monadic $fn)
    ((monad-bind-fn $monad) $monadic $fn))

  (define (monad-map $monad $monadic $fn)
    (monad-bind $monad $monadic
      (lambda ($value)
        (monad-return $monad ($fn $value)))))

  (define (monad-sequence $monad $monadic-list)
    (cond
      ((null? $monadic-list) (monad-return $monad (list)))
      (else 
        (monad-bind $monad (car $monadic-list)
          (lambda ($car)
            (monad-map $monad (monad-sequence $monad (cdr $monadic-list))
              (lambda ($cdr)
                (cons $car $cdr))))))))

  (define (monad-lift $monad $fn . $monadic-args)
    (monad-map $monad (monad-sequence $monad $monadic-args)
      (lambda ($args)
        (apply $fn $args))))

  (define option-monad 
    (monad
      (lambda ($value) 
        (or $value (throw option-monad-return $value)))
      (lambda ($monadic $fn)
        (and $monadic ($fn $monadic)))))
)