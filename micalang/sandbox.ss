(import (micalang mica))

(mica-print
  #t
  #f
  12
  3.14
  'a
  "foo"
  'foo
  (+ 1 2)
  (pi (x type) x)
  (lambda (a number) (b number) (+ a b))
  ((lambda (a number) (b number) (+ a b)) 10 20)
  (let
    (połącz (native (pi string string string) (prim string-append a b)))
    (połącz "foo" "bar")))

