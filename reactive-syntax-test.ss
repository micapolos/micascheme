(import (micascheme) (reactive-syntax))

(define-aux-keyword counter)

(define (counter-context)
  (context
    (lambda ($id)
      (and
        (free-identifier=? $id #`counter)
        (reactive-counter)))))

(check
  (equal?
    (reactive-datum
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
    (reactive-datum
      (syntax-reactive
        (counter-context)
        #`counter))
    `(reactive
      (declarations (define $counter -1))
      (initializers)
      (updaters (set! $counter (+ $counter 1)))
      (value $counter))))

(check
  (equal?
    (reactive-datum
      (syntax-reactive
        (counter-context)
        #`(lets
          ($n counter)
          (+ $n $n))))
    `(reactive
      (declarations (define $counter -1))
      (initializers)
      (updaters (set! $counter (+ $counter 1)))
      (value (let (($n $counter)) (+ $n $n))))))

(check
  (equal?
    (reactive-vector (reactive-counter) 5)
    (vector 0 1 2 3 4)))

(check
  (equal?
    (reactive-vector
      (syntax-reactive
        (counter-context)
        #`(lets
          ($n counter)
          (+ $n $n)))
      5)
    (vector 0 2 4 6 8)))
