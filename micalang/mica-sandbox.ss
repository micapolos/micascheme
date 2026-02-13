(import (micalang mica))

(print
  (local x "Hello, ")
  (local y "world")
  (local z (string-append x y))
  (string-append z "!"))
