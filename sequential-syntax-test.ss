(import (micascheme) (sequential-syntax))

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
    (syntax->datum
      (sequential-syntax
        (sequential
          (deps
            (stack #`def1 #`def2)
            (stack #`fn1 #`fn2))
          #`val)))
    `(sequential
      (deps
        (stack (syntax def1) (syntax def2))
        (stack (syntax fn1) (syntax fn2)))
      (syntax val))))

(check
  (equal?
    (sequential->datum
      (syntax-sequential
        (empty-context)
        #`128))
    `(sequential
      (declarations)
      (updaters)
      (value 128))))

(check
  (equal?
    (sequential->datum
      (sequential-list
        (list
          (sequential (deps (stack #`def1) (stack #`update1)) #`value1)
          (sequential (deps (stack #`def2) (stack #`update2)) #`value2)
          (sequential (deps (stack #`def3) (stack #`update3)) #`value3))))
    `(sequential
      (declarations def1 def2 def3)
      (updaters update1 update2 update3)
      (value (value1 value2 value3)))))

(check
  (equal?
    (sequential->datum
      (syntax-sequential
        (empty-context)
        #`(sequence 0 n (+ n 1))))
    `(sequential
      (declarations (define $n 0))
      (updaters (set! $n (+ $n 1)))
      (value $n))))

(check
  (equal?
    (sequential->datum
      (syntax-sequential (empty-context)
        #`(apply
          (lambda (d) (sequence 0 n (+ n d)))
          (sequence 0 v (+ v 1)))))
    `(sequential
      (declarations
        (define $v 0)
        (define $n 0))
      (updaters
        (set! $v (+ $v 1))
        (set! $n (+ $n $v)))
      (value $n))))

(check
  (equal?
    (sequential->datum
      (syntax-sequential
        (empty-context)
        #`(lets
          (counter (sequence 0 n (+ n 1)))
          (+ counter counter))))
    `(sequential
      (declarations
        (define $n 0)
        (define $counter $n))
      (updaters
        (set! $n (+ $n 1))
        (set! $counter $n))
      (value (+ $counter $counter)))))

(check
  (equal?
    (sequential->datum
      (syntax-sequential
        (empty-context)
        #`(lets
          ((osc dx) (sequence 0 x (fract (+ x dx))))
          (osc 0.25))))
    `(sequential
      (declarations (define $x 0))
      (updaters (set! $x (fract (+ $x 0.25))))
      (value $x))))

(check
  (equal?
    (sequential->datum
      (syntax-sequential
        (empty-context)
        #`(lets
          (counter (sequence 0 c (+ c 1)))
          (sequence 0 x (+ x counter)))))
    `(sequential
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
        #`(define counter (sequence 0 n (+ n 1)))))
    `(begin
      (define-aux-keyword counter)
      (define-property counter sequential
        (sequential
          (deps
            (stack #'(define $n 0))
            (stack #'(set! $n (+ $n 1))))
          #'$n)))))

(check
  (equal?
    (syntax->datum
      (syntax-transform
        (empty-context)
        #`(define osc (lambda (dt) (sequence 0 t (+ t dt))))))
    `(begin
      (define-aux-keyword osc)
      (define-property osc sequential
        (template
          (stack)
          (list (syntax dt))
          (syntax (sequence 0 t (+ t dt))))))))

(check
  (equal?
    (syntax->datum
      (syntax-transform
        (empty-context)
        #`(define (osc dt) (sequence 0 t (+ t dt)))))
    `(begin
      (define-aux-keyword osc)
      (define-property osc sequential
        (template
          (stack)
          (list (syntax dt))
          (syntax (sequence 0 t (+ t dt))))))))

(check
  (equal?
    (syntax->datum
      (syntax-transform
        (empty-context)
        #`(sequence 0 x (+ x 1))))
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
    (sequential->vector
      (syntax-sequential
        (empty-context)
        #`(sequence 0 $val (+ $val 1)))
      5)
    (vector 0 1 2 3 4)))

(check
  (equal?
    (sequential->vector
      (syntax-sequential
        (empty-context)
        #`(+
          (sequence 0 c (+ c 1))
          (sequence 0 x (+ x 100))))
      5)
    (vector 0 101 202 303 404)))


(check
  (equal?
    (sequential->vector (sequential-counter) 5)
    (vector 0 1 2 3 4)))

(check
  (equal?
    (sequential->vector (sequential-osc (pure-sequential 0.25)) 6)
    (vector 0.0 0.25 0.5 0.75 0.0 0.25)))

(check
  (equal?
    (sequential->vector
      (syntax-sequential
        (empty-context)
        #`(lets
          ($counter (sequence 0 c (+ c 1)))
          (+ $counter $counter)))
      5)
    (vector 0 2 4 6 8)))

(check
  (equal?
    (sequential->vector
      (syntax-sequential
        (empty-context)
        #`(lets
          ($counter (sequence 0 $n (+ $n 1)))
          (sequence 0 $acc (+ $acc $counter))))
      5)
    (vector 0 1 3 6 10)))

(check
  (equal?
    (sequential->vector
      (syntax-sequential
        (empty-context)
        #`(apply
          (lambda (dt) (sequence 0 t (+ t dt)))
          (sequence 0 x (+ x 1))))
      5)
    (vector 0 1 3 6 10)))
