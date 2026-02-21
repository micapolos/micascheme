(import (leo2 code))

(check-datum->code=? #t "#t")
(check-datum->code=? #f "#f")

(check-datum->code=? 0 "0")
(check-datum->code=? 1 "1")
(check-datum->code=? -1 "-1")
(check-datum->code=? 3.14 "3.14")

(check-datum->code=? #\space "#\\space")
(check-datum->code=? "" "\"\"")
(check-datum->code=? "foo" "\"foo\"")

