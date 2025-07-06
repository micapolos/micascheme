(import (typico base) (typico resolver))

(check
  (equal?
    (resolve (empty-resolver) 1)
    1))

(lets
  ($resolver
    (resolver-append
      (resolver ($resolver $value)
        (syntax-case? $value (if-zero?)
          ((if-zero? a b c)
            (if
              (zero? (resolve $resolver #'a))
              (resolve $resolver #'b)
              (resolve $resolver #'c)))))
      (resolver ($resolver $value)
        (syntax-case? $value (+)
          (+ +)))
      (resolver ($resolver $value)
        (syntax-case? $value ()
          ((fn arg ...)
            (apply
              (resolve $resolver #'fn)
              (map (partial resolve $resolver) #'(arg ...))))))))
  (run
    (check
      (equal?
        (resolve $resolver 123)
        123))
    (check
      (equal?
        (resolve $resolver +)
        +))
    (check
      (equal?
        (resolve $resolver '(+ 1 2))
        3))
    (check
      (equal?
        (resolve $resolver '(if-zero? (+ 0 0) (+ 1 1) (+ 2 2)))
        2))
    (check
      (equal?
        (resolve $resolver '(if-zero? (+ 0 1) (+ 1 1) (+ 2 2)))
        4))))

