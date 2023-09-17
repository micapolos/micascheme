(import (micascheme) (reactive-syntax))

(define-aux-keyword counter)
(define-aux-keyword osc)

(define (test-context)
  (context
    (stack
      (cons #`$string "bar")
      (cons #`$number 128)
      (cons #`$string "foo"))
    (lambda ($id)
      (cond
        ((free-identifier=? $id #`counter) (reactive-counter))
        ((free-identifier=? $id #`osc) reactive-osc)
        (else #f)))))

(check (equal? (context-ref (test-context) #`$number) 128))
(check (equal? (context-ref (test-context) #`$string) "foo"))
(check (equal? (context-ref (test-context) #`osc) reactive-osc))
(check (equal? (context-ref (test-context) #`$absent) #f))

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
      (value (let (($$n $counter)) (+ $$n $$n))))))

(check
  (equal?
    (reactive->vector
      (syntax-reactive
        (empty-context)
        #`(value $val 0 (+ $val 1)))
      5)
    (vector 0 1 2 3 4)))

(check
  (equal?
    (reactive->vector
      (syntax-reactive
        (empty-context)
        #`(+
          (value c 0 (+ c 1))
          (value x 0 (+ x 100))))
      5)
    (vector 0 101 202 303 404)))

(check
  (equal?
    (reactive->datum
      (syntax-reactive
        (empty-context)
        #`(lets
          ($counter (value c 0 (+ c 1)))
          (value x 0 (+ x $counter)))))
    `(reactive
      (declarations (define $c) (define $x))
      (initializers (set! $c 0) (set! $x 0))
      (updaters (set! $c (+ $c 1)) (set! $x (+ $x $$counter)))
      (value (let (($$counter $c)) $x)))))

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
