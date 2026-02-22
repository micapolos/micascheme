(import
  (leo2 base)
  (leo2 term)
  (leo2 dependent))

(check
  (procedure-dependent?
    (lambda (x) x)))

(check
  (not
    (procedure-dependent?
      (lambda (_) (native 1)))))

(check
  (signature-dependent?
    (signature (type 0)
      (lambda (x) x))))

(check
  (not
    (signature-dependent?
      (signature (type 0)
        (lambda (_) (native 1))))))

(check
  (recursion-dependent?
    (recursion (lambda (x) x))))

(check
  (not
    (recursion-dependent?
      (recursion (lambda (_) (native 1))))))

