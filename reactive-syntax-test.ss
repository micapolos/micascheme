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
          (n counter)
          (+ n n))))
    `(reactive
      (declarations
        (define $counter)
        (define $n))
      (initializers
        (set! $counter 0)
        (set! $n $counter))
      (updaters
        (set! $counter (+ $counter 1))
        (set! $n $counter))
      (value (+ $n $n)))))

(check
  (equal?
    (reactive->datum
      (syntax-reactive
        (empty-context)
        #`(lets
          (counter (iterator c 0 (+ c 1)))
          (iterator x 0 (+ x counter)))))
    `(reactive
      (declarations
        (define $c)
        (define $counter)
        (define $x))
      (initializers
        (set! $c 0)
        (set! $counter $c)
        (set! $x 0))
      (updaters
        (set! $c (+ $c 1))
        (set! $counter $c)
        (set! $x (+ $x $counter)))
      (value $x))))

(check
  (equal?
    (syntax->datum
      (syntax-transform
        (empty-context)
        #`(define counter (iterator n 0 (+ n 1)))))
    `(begin
      (define-aux-keyword counter)
      (define-property counter reactive
        (reactive
          (unit
            (stack #'(define $n))
            (stack #'(set! $n 0))
            (stack #'(set! $n (+ $n 1))))
          #'$n)))))

(check
  (equal?
    (syntax->datum
      (syntax-transform
        (empty-context)
        #`(iterator x 0 (+ x 1))))
    `(writeln
      (let ()
        (define $vector (make-vector 10))
        (define $x)
        (set! $x 0)
        (do!
          (($index 0 (+ $index 1)))
          ((= $index 10) $vector)
          (vector-set! $vector $index $x)
          (set! $x (+ $x 1)))))))

(check
  (equal?
    (reactive->vector
      (syntax-reactive
        (empty-context)
        #`(iterator $val 0 (+ $val 1)))
      5)
    (vector 0 1 2 3 4)))

(check
  (equal?
    (reactive->vector
      (syntax-reactive
        (empty-context)
        #`(+
          (iterator c 0 (+ c 1))
          (iterator x 0 (+ x 100))))
      5)
    (vector 0 101 202 303 404)))


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
          ($counter (iterator c 0 (+ c 1)))
          (+ $counter $counter)))
      5)
    (vector 0 2 4 6 8)))

(check
  (equal?
    (reactive->vector
      (syntax-reactive
        (test-context)
        #`(lets
          ($counter (iterator $n 0 (+ $n 1)))
          (iterator $acc 0 (+ $acc $counter))))
      5)
    (vector 0 1 3 6 10)))
