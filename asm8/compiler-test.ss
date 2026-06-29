(import (scheme) (check) (stack) (asm8 compiler))

(define var-count 0)

(define (gen)
  (set! var-count (+ var-count 1))
  (string->symbol (string-append "$" (number->string var-count))))

(set! var-count 0)
(check
  (equal?
    (compile-op gen (list) 0 (lambda () 10) 1)
    (stack
      (entry 1 10))))

(set! var-count 0)
(check
  (equal?
    (compile-op
      gen
      (stack (entry 1 10))
      1
      (lambda (v0) `(inc ,v0))
      1)
    (stack
      (entry 1
        '(smart-let 10 ($1)
          (smart-values 1 (inc $1)))))))

(set! var-count 0)
(check
  (equal?
    (compile-op
      gen
      (stack (entry 1 10))
      1
      (lambda (v0) `(log ,v0))
      0)
    (stack
      (entry 0
        '(smart-let 10 ($1)
          (smart-values 0 (log $1)))))))

(set! var-count 0)
(check
  (equal?
    (compile-op
      gen
      (stack
        (entry 1 10)
        (entry 1 20))
      2
      (lambda (v0 v1) `(+ ,v0 ,v1))
      1)
    (stack
      (entry 1
        '(smart-let 10 ($2)
          (smart-values 1
            (smart-let 20 ($1)
              (smart-values 1
                (+ $1 $2)))))))))

(set! var-count 0)
(check
  (equal?
    (compile-op
      gen
      (stack
        (entry 1 10)
        (entry 0 '(void)))
      1
      (lambda (v0) `(inc ,v0))
      1)
    (stack
      (entry 1
        '(smart-let 10 ($1)
          (smart-values 1
            (smart-let (void) ()
              (smart-values 1 (inc $1)))))))))

(set! var-count 0)
(check
  (equal?
    (compile-op
      gen
      (stack
        (entry 1 10)
        (entry 1 20))
      2
      (lambda (v0 v1) `(div/rem ,v0 ,v1))
      2)
    (stack
      (entry 2
        '(smart-let 10 ($2)
          (smart-values 2
            (smart-let 20 ($1)
              (smart-values 2
                (div/rem $1 $2)))))))))

(set! var-count 0)
(check
  (equal?
    (compile-op
      gen
      (stack (entry 2 '(values 10 20)))
      2
      (lambda (v0 v1) `(+ ,v0 ,v1))
      1)
    (stack
      (entry 1
        '(smart-let (values 10 20) ($1 $2)
          (smart-values 1
            (+ $1 $2)))))))

; (set! var-count 0)
; (check
;   (equal?
;     (compile-op
;       gen
;       (stack (entry 2 '(values 10 20)))
;       1
;       (lambda (v0) `(inc ,v0))
;       1)
;     (stack
;       (entry 1
;         '(smart-let (values 10 20) ($1 $2)
;           (smart-values 1
;             (+ $1 $2)))))))
