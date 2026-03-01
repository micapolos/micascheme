(import
  (leo2 base)
  (leo2 term)
  (leo2 dependent))

(check
  (lambda-dependent?
    (lambda (x) x)))

(check
  (not
    (lambda-dependent?
      (lambda (_) (native 1)))))

(check
  (lambda-type-dependent?
    (lambda-type (type 0)
      (lambda (x) x))))

(check
  (not
    (lambda-type-dependent?
      (lambda-type (type 0)
        (lambda (_) (native 1))))))

(check
  (recursion-dependent?
    (recursion (lambda (x) x))))

(check
  (not
    (recursion-dependent?
      (recursion (lambda (_) (native 1))))))

