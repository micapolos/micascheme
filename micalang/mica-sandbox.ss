(import (micalang mica))

(print
  (define x "Hello, ")
  (define y "world")
  (define z (string-append x y))
  (string-append z "!"))
