(import (scheme) (check) (asm8 compiler))

(define var-count 0)

(define (gen)
  (set! var-count (+ var-count 1))
  (string->symbol (string-append "$" (number->string var-count))))

(set! var-count 0)
(check
  (equal?
    (compile-op gen (list) 0 (lambda () 10) 1)
    (list
      (cons 1 10))))

(set! var-count 0)
(check
  (equal?
    (compile-op
      gen
      (list (cons 1 10))
      1
      (lambda (v0) `(inc ,v0))
      1)
    (list
      (cons 1
        '(smart-let 10 ($1)
          (inc $1))))))

(set! var-count 0)
(check
  (equal?
    (compile-op
      gen
      (list (cons 1 10))
      1
      (lambda (v0) `(log ,v0))
      0)
    (list
      (cons 0
        '(smart-let 10 ($1)
          (log $1))))))

(set! var-count 0)
(check
  (equal?
    (compile-op
      gen
      (list
        (cons 1 20)
        (cons 1 10))
      2
      (lambda (v0 v1) `(+ ,v0 ,v1))
      1)
    (list
      (cons 1
        '(smart-let 10 ($2)
          (smart-let 20 ($1)
            (+ $1 $2)))))))

(set! var-count 0)
(check
  (equal?
    (compile-op
      gen
      (list (cons 0 '(void)) (cons 1 10))
      1
      (lambda (v0) `(inc ,v0))
      1)
    (list
      (cons 1
        '(smart-let 10 ($1)
          (smart-let (void) ()
            (inc $1)))))))

(set! var-count 0)
(check
  (equal?
    (compile-op
      gen
      (list
        (cons 1 20)
        (cons 1 10))
      2
      (lambda (v0 v1) `(div/rem ,v0 ,v1))
      2)
    (list
      (cons 2
        '(smart-let 10 ($2)
          (smart-let 20 ($1)
            (div/rem $1 $2)))))))

(set! var-count 0)
(check
  (equal?
    (compile-op
      gen
      (list
        (cons 2 '(values 10 20)))
      2
      (lambda (v0 v1) `(+ ,v0 ,v1))
      1)
    (list
      (cons 1
        '(smart-let (values 10 20) ($1 $2)
          (+ $1 $2))))))
