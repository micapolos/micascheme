(import (leo2 base) (leo2 term) (leo2 normalize))

(define (%string-length $lhs)
  (if (native? $lhs)
    (native 0 (string-length (native-value $lhs)) (stack))
    (native 0 %string-length (stack $lhs))))

(define (%string-append $lhs $rhs)
  (if (and (native? $lhs) (native? $rhs))
    (native 0 (string-append (native-value $lhs) (native-value $rhs)) (stack))
    (native 0 %string-append (stack $lhs $rhs))))

(check
  (equal?
    (normalize (stack) (native 0 10 (list)))
    (native 0 10 (stack))))

(check
  (equal?
    (normalize (stack)
      (application
        (native 1 %string-length (stack))
        (native 0 "foo" (stack))))
    (native 0 3 (stack))))

(check
  (equal?
    (normalize (stack)
      (application
        (native 2 %string-append (stack))
        (native 0 "foo" (stack))))
    (native 1 %string-append (stack (native 0 "foo" (stack))))))

(check
  (equal?
    (normalize (stack)
      (abstraction
        (native 0 'a-number (stack))
        (application
          (native 1 %string-length (stack))
          (variable 0))))
    (abstraction
      (native 0 'a-number (stack))
      (native 0 %string-length (stack (variable 0))))))

(check
  (equal?
    (normalize (stack)
      (application
        (application
          (native 2 %string-append (stack))
          (native 0 "foo" (stack)))
        (native 0 "bar" (stack))))
    (native 0 "foobar" (stack))))

(check
  (equal?
    (normalize (stack)
      (abstraction
        (native 0 'a-string (stack))
        (application
          (application
            (native 2 %string-append (stack))
            (native 0 "foo" (stack)))
          (native 0 "bar" (stack)))))
    (abstraction
      (native 0 'a-string (stack))
      (native 0 "foobar" (stack)))))

(check
  (equal?
    (normalize (stack)
      (abstraction
        (native 0 'a-string (stack))
        (application
          (application
            (native 2 %string-append (stack))
            (variable 0))
          (native 0 "bar" (stack)))))
    (abstraction
      (native 0 'a-string (stack))
      (native 0 %string-append
        (stack
          (variable 0)
          (native 0 "bar" (stack)))))))


