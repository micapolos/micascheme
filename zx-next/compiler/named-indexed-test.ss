(import (zx-next compiler named-indexed))

; indexed
(check-named->indexed
  (() (indexed foo))
  (() foo))


; free variables
(check-named->indexed
  (() a)
  ((a) a))

(check-named->indexed
  (() (a b c))
  ((a b c) (a b c)))

(check-named->indexed
  (() (a b (a b c)))
  ((a b c) (a b (a b c))))

; bound variables
(check-named->indexed
  ((v1 v2 v3) v1)
  (() 2))

(check-named->indexed
  ((v1 v2 v3) v2)
  (() 1))

(check-named->indexed
  ((v1 v2 v3) v3)
  (() 0))

; local bindings
(check-named->indexed
  (() (lets (v1 a) (v2 b) (+ v1 v2)))
  ((a b +) (lets 2 (+ 1 0))))
