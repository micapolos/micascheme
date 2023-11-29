(import (micascheme) (io))

(define $current 0)

(check
  (equal?
    (app
      (lets
        (in io
          ($var1 (io-var "foo"))
          ($var2 (io-var "bar"))
          ($value1 (io-get $var1))
          ($value2 (io-get $var2))
          (do (io-set $var1 (string-append $value1 "+")))
          ($value1 (io-get $var1))
          (do (io-set $var2 (string-append $value1 $value2)))
          (io-get $var2))))
    "foo+bar"))
