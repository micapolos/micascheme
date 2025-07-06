(import (typico base) (typico core resolvers))

(check-resolves boolean-resolver #t (boolean #t))
(check-resolves boolean-resolver #f (boolean #f))
(check-does-not-resolve boolean-resolver 123)

(check-resolves integer-resolver 123 (integer 123))
(check-does-not-resolve integer-resolver #f)

(check-resolves char-resolver #\a (char #\a))
(check-does-not-resolve char-resolver 123)

(check-resolves string-resolver "foo" (string "foo"))
(check-does-not-resolve string-resolver 123)

(check-resolves symbol-resolver foo (symbol foo))
(check-does-not-resolve symbol-resolver 123)
