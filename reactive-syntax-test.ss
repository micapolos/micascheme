(import (micascheme) (reactive-syntax))

(define-aux-keyword counter)
(define-aux-keyword osc)

(define (test-context)
  (context
    (lambda ($id)
      (cond
        ((free-identifier=? $id #`counter) (reactive-counter))
        ((free-identifier=? $id #`osc) reactive-osc)
        (else #f)))))

(check
  (equal?
    (reactive->datum
      (syntax-reactive
        (empty-context)
        #`128))
    `(reactive
      (declarations)
      (initializers)
      (updaters)
      (value 128))))

(check
  (equal?
    (reactive->datum
      (syntax-reactive
        (test-context)
        #`counter))
    `(reactive
      (declarations (define $counter))
      (initializers (set! $counter 0))
      (updaters (set! $counter (+ $counter 1)))
      (value $counter))))

(check
  (equal?
    (reactive->datum
      (syntax-reactive
        (test-context)
        #`(lets
          ($n counter)
          (+ $n $n))))
    `(reactive
      (declarations (define $counter))
      (initializers (set! $counter 0))
      (updaters (set! $counter (+ $counter 1)))
      (value (let (($n $counter)) (+ $n $n))))))

(check
  (equal?
    (reactive->vector (reactive-counter) 5)
    (vector 0 1 2 3 4)))

(check
  (equal?
    (reactive->vector (reactive-osc (pure-reactive 0.25)) 6)
    (vector 0.0 0.25 0.5 0.75 0.0 0.25)))

(check
  (equal?
    (reactive->vector
      (syntax-reactive
        (test-context)
        #`(lets
          ($n counter)
          (+ $n $n)))
      5)
    (vector 0 2 4 6 8)))
