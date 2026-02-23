(import
  (prefix (leo2 base) %)
  (leo2 lang))

(check-lang
  nothing
  anything
  (variable 3)
  (native 10)
  (native-apply "foo" (variable 3) (variable 4))
  (lambda v0 (variable 0))
  (lambda (v0 (variable 2)) (variable 3))
  (recursive v0 (lambda (v1 (variable 2)) (apply (variable 3) (variable 4))))
  (apply (variable 0) (variable 1))
  (if (variable 0) (variable 1) (variable 2))
  (labeled (variable 0) (variable 1))
  (evaluated (variable 0))
  (typed (variable 0) (variable 1)))
