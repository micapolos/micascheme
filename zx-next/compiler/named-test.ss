(import
  (zx-next compiler named)
  (zx-next compiler named-keywords)
  (syntax lookup))

(check-named (empty-lookup)
  ((a b c) (d e f) 1 a)
  (%%arg 0))

(check-named (empty-lookup)
  ((a b c) (d e f) 1 c)
  (%%arg 2))

(check-named (empty-lookup)
  ((a b c) (d e f) 1 d)
  (%%local 2))

(check-named (empty-lookup)
  ((a b c) (d e f) 1 f)
  (%%local 0))

(check-named (empty-lookup)
  (() () 1 (lets (a 1 #x12) (b 2 #x1234) a))
  123)
