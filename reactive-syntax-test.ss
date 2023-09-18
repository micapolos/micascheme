(import (micascheme) (reactive-syntax))

(define-aux-keyword counter)
(define-aux-keyword osc)

(let ()
  (define $context
    (context
      (stack
        (cons #`$string "bar")
        (cons #`$number 128)
        (cons #`$string "foo"))
      (lambda ($id)
        (cond
          ((free-identifier=? $id #`$char) #\a)
          (else #f)))))

  (check (equal? (context-ref $context #`$number) 128))
  (check (equal? (context-ref $context #`$string) "foo"))
  (check (equal? (context-ref $context #`$char) #\a))
  (check (equal? (context-ref $context #`$absent) #f)))

(check
  (equal?
    (reactive->datum
      (syntax-reactive
        (empty-context)
        #`128))
    `(reactive
      (declarations)
      (updaters)
      (value 128))))

(check
  (equal?
    (reactive->datum
      (reactive-list
        (list
          (reactive (deps (stack #`def1) (stack #`update1)) #`value1)
          (reactive (deps (stack #`def2) (stack #`update2)) #`value2)
          (reactive (deps (stack #`def3) (stack #`update3)) #`value3))))
    `(reactive
      (declarations def1 def2 def3)
      (updaters update1 update2 update3)
      (value (value1 value2 value3)))))

(check
  (equal?
    (reactive->datum
      (syntax-reactive
        (empty-context)
        #`(unit n 0 (+ n 1))))
    `(reactive
      (declarations (define $n 0))
      (updaters (set! $n (+ $n 1)))
      (value $n))))

(check
  (equal?
    (reactive->datum
      (
        (syntax-reactive
          (empty-context)
          #`(lambda (d) (unit n 0 (+ n d))))
        (reactive
          (deps
            (stack #`(define $arg 0))
            (stack #`(set! $arg (+ $arg 1))))
          #`$arg)))
    `(reactive
      (declarations
        (define $arg 0)
        (define $n 0))
      (updaters
        (set! $arg (+ $arg 1))
        (set! $n (+ $n $arg)))
      (value $n))))

(check
  (equal?
    (reactive->datum
      (syntax-reactive
        (empty-context)
        #`(apply
          (lambda (d) (unit n 0 (+ n d)))
          (unit x 0 (+ x 1)))))
    `(reactive
      (declarations
        (define $x 0)
        (define $n 0))
      (updaters
        (set! $x (+ $x 1))
        (set! $n (+ $n $x)))
      (value $n))))

(check
  (equal?
    (reactive->datum
      (syntax-reactive
        (empty-context)
        #`(lets
          (counter (unit n 0 (+ n 1)))
          (+ counter counter))))
    `(reactive
      (declarations
        (define $n 0)
        (define $counter $n))
      (updaters
        (set! $n (+ $n 1))
        (set! $counter $n))
      (value (+ $counter $counter)))))

(check
  (equal?
    (reactive->datum
      (syntax-reactive
        (empty-context)
        #`(lets
          (counter (unit c 0 (+ c 1)))
          (unit x 0 (+ x counter)))))
    `(reactive
      (declarations
        (define $c 0)
        (define $counter $c)
        (define $x 0))
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
        #`(define counter (unit n 0 (+ n 1)))))
    `(begin
      (define-aux-keyword counter)
      (define-property counter reactive
        (reactive
          (deps
            (stack #'(define $n 0))
            (stack #'(set! $n (+ $n 1))))
          #'$n)))))

(check
  (equal?
    (syntax->datum
      (syntax-transform
        (empty-context)
        #`(unit x 0 (+ x 1))))
    `(writeln
      (let ()
        (define $vector (make-vector 10))
        (define $x 0)
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
        #`(unit $val 0 (+ $val 1)))
      5)
    (vector 0 1 2 3 4)))

(check
  (equal?
    (reactive->vector
      (syntax-reactive
        (empty-context)
        #`(+
          (unit c 0 (+ c 1))
          (unit x 0 (+ x 100))))
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
        (empty-context)
        #`(lets
          ($counter (unit c 0 (+ c 1)))
          (+ $counter $counter)))
      5)
    (vector 0 2 4 6 8)))

(check
  (equal?
    (reactive->vector
      (syntax-reactive
        (empty-context)
        #`(lets
          ($counter (unit $n 0 (+ $n 1)))
          (unit $acc 0 (+ $acc $counter))))
      5)
    (vector 0 1 3 6 10)))
