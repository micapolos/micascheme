(import
  (leo2 base)
  (leo2 term)
  (leo2 stdlib)
  (leo2 dependent))

(check
  (abstraction-dependent?
    (abstraction (lambda (x) x))))

(check
  (not
    (abstraction-dependent?
      (abstraction (lambda (x) (number-term 1))))))

(check
  (abstraction-type-dependent?
    (abstraction-type
      (variable-term string-type 'x)
      (lambda (x) x))))

(check
  (not
    (abstraction-type-dependent?
      (abstraction-type
        (variable-term string-type 'x)
        (lambda (x) (number-term 1))))))

(check
  (recursion-dependent?
    (recursion (lambda (x) x))))

(check
  (not
    (recursion-dependent?
      (recursion (lambda (x) (number-term 1))))))

