(import
  (leo2 base)
  (leo2 term)
  (leo2 normalize)
  (leo2 datum)
  (curry))

(define (unreachable $value)
  (application (native raise) (native $value)))

(define (application-2 $fn $lhs $rhs)
  (application (application $fn $lhs) $rhs))

(check-term-datum=?
  (normalize (native 123))
  (native 123))

(check-term-datum=?
  (normalize (application (native number->string) (native 123)))
  (native "123"))

(check-term-datum=?
  (normalize (application-2 (native curry-) (native 30) (native 20)))
  (native 10))

(check-term-datum=?
  (normalize
    (branch
      (native #t)
      (native "true")
      (unreachable "false")))
  (native "true"))

(check-term-datum=?
  (normalize
    (branch
      (native #f)
      (unreachable "true")
      (native "false")))
  (native "false"))

(check-term-datum=?
  (normalize
    (branch
      (variable 0)
      (application (native number->string) (native 10))
      (application (native number->string) (native 20))))
  (branch
    (variable 0)
    (native "10")
    (native "20")))

(check-term-datum=?
  (normalize
    (application
      (recursion
        (lambda ($fn)
          (lambda ($n)
            (branch (application (native zero?) $n)
              (native "OK")
              (application $fn
                (application-2 (native curry-) $n (native 1)))))))
      (native 100)))
  (native "OK"))

; (check-term-datum=?
;   (normalize
;     (application
;       (recursion
;         (lambda ($fib)
;           (lambda ($n)
;             (branch (application-2 (native curry<) $n (native 2))
;               $n
;               (application $fib
;                 (application-2 (native curry+)
;                   (application-2 (native curry-) $n (native 1))
;                   (application-2 (native curry-) $n (native 2))))))))
;       (native 4)))
;   (native 55))
