(import
  (leo2 base)
  (leo2 term)
  (leo2 datum))

(check-term->datum=? nothing nothing)
(check-term->datum=? anything anything)

(check-term->datum=? (type 12) (type 12))

(check-term->datum=?
  (native string-append)
  (native ,string-append))

(check-term->datum=?
  (native-application string-append (list (native "foo") (native "bar")))
  (native-apply ,string-append (native "foo") (native "bar")))

(check-term->datum=? (variable 0) (variable 0))

(check-term->datum=?
  (lambda (x) (application (variable 10) x))
  (lambda v0 (apply (variable 10) (variable 0))))

(check-term->datum=?
  (signature (variable 10) (lambda (x) (application (variable 20) x)))
  (lambda (v0 (variable 10)) (apply (variable 20) (variable 0))))

(check-term->datum=?
  (recursion (lambda (fn) (lambda (x) (application fn x))))
  (recursive v0 (lambda v1 (apply (variable 0) (variable 1)))))

(check-term->datum=?
  (application (variable 0) (variable 1))
  (apply (variable 0) (variable 1)))

(check-term->datum=?
  (branch (variable 0) (variable 1) (variable 2))
  (if (variable 0) (variable 1) (variable 2)))

(check-term->datum=?
  (labeled (variable 0) (native "milk"))
  (labeled (variable 0) (native "milk")))

(check-term->datum=?
  (typed (variable 0) (native "milk"))
  (typed (variable 0) (native "milk")))

(check-term->datum=?
  (evaluated (native "milk"))
  (evaluated (native "milk")))
