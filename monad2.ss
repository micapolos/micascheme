(library (monad2)
  (export
    monad monad? monad-check-fn monad-pure-fn monad-bind-fn
    monad-check? monad-pure monad-bind monad-let
    monad-lets

    timed! timed timed? timed-fn
    timed-of timed-get timed-accelerate timed-bind
    time-timed
    timing

    opt-monad ensuring timing)
  (import (micascheme))

  (data (monad check-fn pure-fn bind-fn))

  (define (monad-check? $monad $value)
    ((monad-check-fn $monad) $value))

  (define (monad-pure $monad $value)
    ((monad-pure-fn $monad) $value))

  (define (monad-bind $monad $value $fn)
    ((monad-bind-fn $monad) $value $fn))

  (define (monad-ensure $monad $value)
    (if (monad-check? $monad $value)
      $value
      (monad-pure $monad $value)))

  (define (monad-let $monad $value $fn)
    (monad-bind $monad
      (monad-ensure $monad $value)
      $fn))

  (define-syntax-case monad-lets ()
    ((_ $monad $result)
      #`(monad-ensure $monad $result))
    ((_ $monad ($var $expr) $case2 ... $result)
      #`(monad-let $monad $expr
        (lambda ($var)
          (monad-lets $monad $case2 ... $result)))))

  (define opt-monad
    (monad
      (lambda ($value) #t)
      (lambda ($value) $value)
      (lambda ($value $fn)
        (and $value ($fn $value)))))

  (define (ensuring $pred)
    (monad
      (lambda ($value) #t)
      (lambda ($value) $value)
      (lambda ($value $fn)
        (cond
          (($pred $value) ($fn $value))
          (else $value)))))

  (data (timed fn))

  (define-syntax-rule (timed! $var $body)
    (timed (lambda ($var) $body)))

  (define (timed-of $value)
    (timed! $time $value))

  (define (timed-get $timed $time)
    ((timed-fn $timed) $time))

  (define (timed-accelerate $timed $ratio)
    (timed! $time
      (timed-get $timed (* $time $ratio))))

  (define time-timed
    (timed! $time $time))

  (define (timed-bind $timed $fn)
    (timed
      (lambda ($time)
        (timed-get
          ($fn (timed-get $timed $time))
          $time))))

  (define timing
    (monad timed? timed-of timed-bind))
)
