(import (scheme) (check) (stack) (asm8 compiler))

(define var-count 0)

(define (gen)
  (set! var-count (+ var-count 1))
  (string->symbol (string-append "$" (number->string var-count))))

(define (op-push $x) (op 0 1 (lambda () $x)))
(define op-log (op 1 0 (lambda ($0) `(log ,$0))))
(define op-inc (op 1 1 (lambda ($0) `(inc ,$0))))
(define op+ (op 2 1 (lambda ($0 $1) `(+ ,$0 ,$1))))
(define op-div/rem (op 2 2 (lambda ($0 $1) `(div/rem ,$0 ,$1))))

(set! var-count 0)
(check
  (equal?
    (compile-op gen (stack) (op-push 10))
    (stack
      (entry 1 10))))

(set! var-count 0)
(check
  (equal?
    (compile-op gen (stack (entry 1 10)) op-inc)
    (stack
      (entry 1
        '(smart-bind 10 ($1)
          (smart-values
            (1 (inc $1))))))))

(set! var-count 0)
(check
  (equal?
    (compile-op
      gen
      (stack (entry 1 10))
      op-log)
    (stack
      (entry 0
        '(smart-bind 10 ($1)
          (smart-values
            (0 (log $1))))))))

(set! var-count 0)
(check
  (equal?
    (compile-op
      gen
      (stack
        (entry 1 10)
        (entry 1 20))
      op+)
    (stack
      (entry 1
        '(smart-bind 10 ($2)
          (smart-values
            (1
              (smart-bind 20 ($1)
                (smart-values (1 (+ $1 $2)))))))))))

(set! var-count 0)
(check
  (equal?
    (compile-op
      gen
      (stack
        (entry 1 10)
        (entry 0 '(void)))
      op-inc)
    (stack
      (entry 1
        '(smart-bind 10 ($1)
          (smart-values
            (1
              (smart-bind (void) ()
                (smart-values (1 (inc $1)))))))))))

(set! var-count 0)
(check
  (equal?
    (compile-op
      gen
      (stack
        (entry 1 10)
        (entry 1 20))
      op-div/rem)
    (stack
      (entry 2
        '(smart-bind 10 ($2)
          (smart-values
            (2
              (smart-bind 20 ($1)
                (smart-values
                  (2
                    (div/rem $1 $2)))))))))))

(set! var-count 0)
(check
  (equal?
    (compile-op
      gen
      (stack (entry 2 '(values 10 20)))
      op+)
    (stack
      (entry 1
        '(smart-bind (values 10 20) ($1 $2)
          (smart-values
            (1
              (+ $1 $2))))))))

; (set! var-count 0)
; (check
;   (equal?
;     (compile-op
;       gen
;       (stack (entry 5 '(values 1 2 3 4 5)))
;       op+)
;     (stack
;       (entry 4
;         '(smart-bind
;           (values 1 2 3 4 5)
;           ($1 $2 $3 $4 $5)
;           (smart-values
;             (3 $1 $2 $3)
;             (1 (+ $4 $5))))))))
