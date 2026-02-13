(import (micalang mica))

(mica-print
  (
    (let
      (val x 10)
      (val y 20)
      (input z any-number)
      (+ (+ x y) z))
    30))
