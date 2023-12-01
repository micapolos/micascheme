(import (micascheme) (monad) (lets))

; monad-pure

(check 
  (equal? 
    (monad-pure option-monad "foo") 
    "foo"))

; monad-bind

(check 
  (equal? 
    (monad-bind option-monad #f
      (lambda ($value) 
        (string-append $value "bar"))) 
    #f))

(check 
  (equal? 
    (monad-bind option-monad "foo" 
      (lambda ($value) 
        (string-append $value "bar"))) 
    "foobar"))

(check 
  (equal? 
    (monad-bind option-monad #f 
      (lambda ($value) #f))
    #f))

(check 
  (equal? 
    (monad-bind option-monad "foo" 
      (lambda ($value) #f))
    #f))

; monad-map

(check 
  (equal? 
    (monad-map option-monad "foo"
      (lambda ($value) 
        (string-append $value "bar")))
    "foobar"))

(check 
  (equal? 
    (monad-map option-monad #f
      (lambda ($value) 
        (string-append $value "bar")))
    #f))

; monad-sequence

(check (equal? (monad-sequence option-monad (list)) (list)))
(check (equal? (monad-sequence option-monad (list "a" "b" "c")) (list "a" "b" "c")))
(check (equal? (monad-sequence option-monad (list "a" #f "c")) #f))

; monad-lift

(check (equal? (monad-lift option-monad string-append "a" "b") "ab"))
(check (equal? (monad-lift option-monad string-append "a" #f) #f))
(check (equal? (monad-lift option-monad string-append #f "b") #f))
(check (equal? (monad-lift option-monad string-append #f #f) #f))

; monad-lets

(check
  (equal?
    (monad-lets (cons-monad `foo) (cons `foo 128))
    (cons `foo 128)))

(check
  (equal?
    (monad-lets (cons-monad `foo) (pure 128))
    (cons `foo 128)))

(check
  (equal?
    (monad-lets (cons-monad `foo)
      ($foo (cons `foo "foo"))
      (pure (string-append $foo "!")))
    (cons `foo "foo!")))

(check
  (equal?
    (monad-lets (cons-monad `foo)
      ($foo (pure "foo"))
      (pure (string-append $foo "!")))
    (cons `foo "foo!")))

(check
  (equal?
    (monad-lets (cons-monad `foo)
      ($foo (pure "foo"))
      ($bar (pure "!"))
      (pure (string-append $foo $bar)))
    (cons `foo "foo!")))

; monad-stack-box

(define cons-monad-1 (cons-monad 1))
(define cons-monad-2 (cons-monad 2))

(check
  (equal?
    (monad-stack-box
      (stack)
      (stack)
      "foo")
    (box "foo")))

(check
  (equal?
    (monad-stack-box
      (stack)
      (stack cons-monad-1)
      "foo")
    (box (cons 1 "foo"))))

(check
  (equal?
    (monad-stack-box
      (stack cons-monad-1)
      (stack)
      "foo")
    #f))

(check
  (equal?
    (monad-stack-box
      (stack)
      (stack cons-monad-1 cons-monad-2)
      "foo")
    (box (cons 2 (cons 1 "foo")))))

(check
  (equal?
    (monad-stack-box
      (stack cons-monad-1 cons-monad-2)
      (stack)
      "foo")
    #f))

(check
  (equal?
    (monad-stack-box
      (stack cons-monad-1)
      (stack cons-monad-1)
      (cons 1 "foo"))
    (box (cons 1 "foo"))))

(check
  (equal?
    (monad-stack-box
      (stack cons-monad-1 cons-monad-2)
      (stack cons-monad-1 cons-monad-2)
      (cons 2 (cons 1 "foo")))
    (box (cons 2 (cons 1 "foo")))))

(check
  (equal?
    (monad-stack-box
      (stack cons-monad-1)
      (stack cons-monad-1 cons-monad-2)
      (cons 1 "foo"))
    (box (cons 2 (cons 1 "foo")))))

(check
  (equal?
    (monad-stack-box
      (stack cons-monad-1)
      (stack cons-monad-2 cons-monad-1)
      (cons 1 "foo"))
    (box (cons 1 (cons 2 "foo")))))

(check
  (equal?
    (monad-stack-box
      (stack cons-monad-1 cons-monad-2)
      (stack cons-monad-2 cons-monad-1)
      (cons 2 (cons 1 "foo")))
    #f))

; listing monad

(check
  (equal?
    (listing-run
      (monad-bind listing-monad (monad-pure listing-monad "foo")
        (lambda ($foo)
          (monad-map listing-monad (monad-pure listing-monad "bar")
            (lambda ($bar)
              (string-append $foo $bar))))))
    (cons "foobar" (list "foobar" "bar" "foo"))))

; monadic

(check
  (equal?
    (listing-run (monadic listing-monad (listing "foo")))
    (cons "foo" (list "foo"))))

(check
  (equal?
    (listing-run (monadic listing-monad (pure "foo")))
    (cons "foo" (list "foo"))))

(check
  (equal?
    (listing-run (monadic listing-monad (lets (pure "foo"))))
    (cons "foo" (list "foo"))))

(define-monadic (plus $a $b)
  (pure (+ $a $b)))

(check
  (equal?
    (listing-run
      (monadic listing-monad
        (lets
          ($foo (pure "foo"))
          ($bar (pure "bar"))
          (pure (string-append $foo $bar)))))
    (cons "foobar" (list "foobar" "bar" "foo"))))

; --- define-monad

(let ()
  (define (linear=? $linear-a $linear-b)
    (and
      (equal? ($linear-a 0.0) ($linear-b 0.0))
      (equal? ($linear-a 1.0) ($linear-b 1.0))
      (equal? ($linear-a 2.0) ($linear-b 2.0))))

  (define-monad linear
    ((pure $value)
      (lambda ($x) $value))
    ((bind $linear $fn)
      (lambda ($x)
        (app ($fn (app $linear $x)) $x))))

  (check (linear=? sin sin))

  (check
    (linear=?
      (lets
        ((linear $sin) sin)
        ((linear $cos) cos)
        (linear
          (+
            (* $sin $sin)
            (* $cos $cos))))
      (lambda ($x)
        (+
          (* (sin $x) (sin $x))
          (* (cos $x) (cos $x))))))
)
