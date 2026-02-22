(import
  (prefix (leo2 base) %)
  (leo2 lang))

(check-lang
  nothing
  anything
  'foo
  (the x)
  (indexed 10 (the x))
  (symbolic good (the x))
  (native 10)
  (native-apply "foo" (the x) (the y))
  (lambda v0 (the v0))
  (lambda (any v0 'foo) (the v0))
  (recursive v0 (lambda (any v1 'foo) (apply (the v0) (the v1))))
  (apply (the fn) (the arg))
  (if (the condition) (the consequent) (the alternate))
  (annotated (the annotation) (the value))
  (evaluated (the value))
  (typed (the type) (the value)))
