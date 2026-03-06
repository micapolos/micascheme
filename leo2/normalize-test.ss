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
      (native 5)))
  (native "OK"))

(check-term-datum=?
  (normalize
    (application
      (recursion
        (lambda ($fib)
          (lambda ($n)
            (branch (application-2 (native curry<) $n (native 2))
              $n
              (application-2 (native curry+)
                (application $fib (application-2 (native curry-) $n (native 1)))
                (application $fib (application-2 (native curry-) $n (native 2))))))))
      (native 10)))
  (native 55))

; selector
(check-term-datum=?
  (normalize selector)
  selector)

; rejector
(check-term-datum=?
  (normalize (rejector selector))
  (rejector selector))

(check-term-datum=?
  (normalize (rejector (rejector selector)))
  (rejector (rejector selector)))

; application with selector
(check-term-datum=?
  (normalize (application selector (native "foo")))
  (native "foo"))

(check-term-datum=?
  (normalize (application (rejector selector) (native "foo")))
  selector)

(check-term-datum=?
  (normalize
    (application
      (application (rejector selector) (native "inner"))
      (native "outer")))
  (native "outer"))

; matcher
(check-term-datum=?
  (normalize (application matcher (native "foo")))
  (switcher (native "foo")))

; switcher
(check-term-datum=?
  (normalize
    (application
      (switcher (native "foo"))
      selector))
  (native "foo"))

; (check-term-datum=?
;   (normalize
;     (application
;       (switcher
;         (application
;           (application selector (native "inner"))
;           (native "outer")))
;       (lambda ($outer)
;         (lambda ($inner)
;           (application
;             (application (native string-append) $inner)
;             $outer)))))
;   (native "innerouter"))
