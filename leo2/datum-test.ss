(import
  (leo2 base)
  (leo2 term)
  (leo2 datum))

(check-term->datum=? nothing nothing)
(check-term->datum=? anything anything)

(check-term->datum=? (type 12) (type 12))
(check-term->datum=? 'foo (symbol foo))

(check-term->datum=?
  (indexed 10 (native "foo"))
  (indexed 10 (native "foo")))

(check-term->datum=?
  (symbolic 'lucky (native 10))
  (symbolic lucky (native 10)))

(check-term->datum=?
  (native string-append)
  (native ,string-append))

(check-term->datum=?
  (native-application string-append (list (native "foo") (native "bar")))
  (native-apply ,string-append (native "foo") (native "bar")))

(check-term->datum=?
  (variable 'x)
  (variable x))

(check-term->datum=?
  (abstraction (lambda (x) (application (variable 'the) x)))
  (lambda v0 (apply (variable the) (variable v0))))

(check-term->datum=?
  (signature (variable 'string) (lambda (x) (application (variable 'the) x)))
  (lambda (v0 (variable string)) (apply (variable the) (variable v0))))

(check-term->datum=?
  (recursion (lambda (fn) (abstraction (lambda(x) (application fn x)))))
  (recursive v0 (lambda v1 (apply (variable v0) (variable v1)))))

(check-term->datum=?
  (application (variable 'x) (variable 'y))
  (apply (variable x) (variable y)))

(check-term->datum=?
  (branch (variable 'a) (variable 'b) (variable 'c))
  (if (variable a) (variable b) (variable c)))

(check-term->datum=?
  (annotated 'good (native "milk"))
  (annotated (symbol good) (native "milk")))

(check-term->datum=?
  (typed 'string (native "milk"))
  (typed (symbol string) (native "milk")))

(check-term->datum=?
  (evaluated (native "milk"))
  (evaluated (native "milk")))
