(import (micascheme) (parser))

(define-syntax-rule (check-parse $parser $string $parsed)
  (check (obj=? (parse $parser $string) $parsed)))

(check-parse (the 123) "" 123)
(check-parse (the 123) "a" (failure (indexed #\a 0)))

(check-parse char "" (eof-object))
(check-parse char "a" #\a)
(check-parse char "ab" (failure (indexed #\b 1)))
