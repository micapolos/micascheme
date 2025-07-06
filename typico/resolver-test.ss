(import (typico base) (typico resolved) (typico resolver))

(check
  (equal?
    (resolve? (empty-resolver) 1)
    #f))

(lets
  ($resolver
    (resolver-append
      (resolver ($resolver $value)
        (syntax-case? $value (if)
          ((if a b c)
            (resolved
              (if
                (resolve $resolver #'a)
                (resolve $resolver #'b)
                (resolve $resolver #'c))))))
      (resolver ($resolver $value)
        (syntax-case? $value (zero?)
          (zero? (resolved zero?))))
      (resolver ($resolver $value)
        (syntax-case? $value (not)
          (not (resolved not))))
      (resolver ($resolver $value)
        (syntax-case? $value (+)
          (+ (resolved +))))
      (resolver ($resolver $value)
        (syntax-case? $value ()
          ((fn arg ...)
            (resolved
              (apply
                (resolve $resolver #'fn)
                (map (partial resolve $resolver) #'(arg ...)))))))))
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
        (resolve $resolver '(if (zero? 0) (+ 1 1) (+ 2 2)))
        2))
    (check
      (equal?
        (resolve $resolver '(if (not (zero? 0)) (+ 1 1) (+ 2 2)))
        4))))

