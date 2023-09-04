(library (monad)
  (export
    monad monad? monad-pure-fn monad-bind-fn
    monad-pure monad-bind monad-map monad-sequence monad-lift monad-apply
    monadic pure define-monadic
    option-monad 
    listing listing-bind listing-run listing-monad)
  (import (micascheme))

  (data (monad pure-fn bind-fn))

  ; combinators

  (define (monad-pure $monad $value)
    ((monad-pure-fn $monad) $value))

  (define (monad-bind $monad $monadic $fn)
    ((monad-bind-fn $monad) $monadic $fn))

  (define (monad-map $monad $monadic $fn)
    (monad-bind $monad $monadic
      (lambda ($value)
        (monad-pure $monad ($fn $value)))))

  (define (monad-sequence $monad $monadic-list)
    (cond
      ((null? $monadic-list) (monad-pure $monad (list)))
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

  (define (monad-apply $monad $monadic-fn . $monadic-args)
    (monad-bind $monad $monadic-fn
      (lambda ($fn)
        (monad-map $monad (monad-sequence $monad $monadic-args)
          (lambda ($args)
            (apply $fn $args))))))

  ; syntaxes

  (define-aux-keyword pure)

  (define-syntax monadic
    (syntax-rules (pure lets)
      ((_ $monad (pure $value))
        (monad-pure $monad $value))
      ((_ $monad (lets ($var $expr) $binding ... $result))
        (monad-bind $monad (monadic $monad $expr)
          (lambda ($var)
            (monadic $monad (lets $binding ... $result)))))
      ((_ $monad (lets $result))
        (monadic $monad $result))
      ((_ $monad $other) 
        $other)))

  (define-syntax define-monadic
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ ($id $param ...) $body)
          (lets
            ($monad (generate-temporary #`monad))
            #`(define $id
              (lambda (#,$monad $param ...) 
                (monadic #,$monad $body))))))))

  ; monads

  (define option-monad 
    (monad
      (lambda ($value) 
        (or $value (throw option-monad-pure $value)))
      (lambda ($option $fn)
        (and $option ($fn $option)))))

  (define (listing $value)
    (lambda ($list) 
      (cons $value (cons $value $list))))

  (define (listing-bind $listing $fn)
    (lambda ($list)
      (lets
        (($value $list) (pair-values ($listing $list)))
        (($fn $value) $list))))

  (define (listing-run $listing) 
    ($listing (list)))

  (define listing-monad 
    (monad listing listing-bind))
)
