(import (micascheme) (reactive-syntax))

(check
  (equal?
    (reactive-datum (syntax-reactive #`128))
    `(reactive
      (declarations)
      (initializers)
      (updaters)
      (value 128))))

(check
  (equal?
    (reactive-datum (syntax-reactive #`counter))
    `(reactive
      (declarations (define $counter -1))
      (initializers)
      (updaters (set! $counter (+ $counter 1)))
      (value $counter))))

(check
  (equal?
    (reactive-datum
      (syntax-reactive
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
    (reactive-vector (counter) 5)
    (vector 0 1 2 3 4)))

(check
  (equal?
    (reactive-vector
      (syntax-reactive
        #`(lets
          ($n counter)
          (+ $n $n)))
      5)
    (vector 0 2 4 6 8)))

