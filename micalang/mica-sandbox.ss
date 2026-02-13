(import (micalang mica))

(print
  (define x "Hello, ")
  (define y "world")
  (define z (string-append x 10))
  (string-append z "!"))
