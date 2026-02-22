(import
  (prefix (leo2 base) %)
  (leo2 lang))

(check-lang
  nothing
  anything
  (var 3)
  (indexed 10 (var 3))
  (symbol foo)
  (symbolic good (var 3))
  (native 10)
  (native-apply "foo" (var 3) (var 4))
  (lambda v0 (var 0))
  (lambda (v0 (var 2)) (var 3))
  (recursive v0 (lambda (v1 (var 2)) (apply (var 3) (var 4))))
  (apply (var 0) (var 1))
  (if (var 0) (var 1) (var 2))
  (annotated (var 0) (var 1))
  (evaluated (var 0))
  (typed (var 0) (var 1)))
