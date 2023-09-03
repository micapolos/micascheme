(import (micascheme) (leo parser))

(check (equal? (parse (parser-of "foo") "") "foo"))
(check (equal? (parse (parser-of "foo") "a") #f))
(check (equal? (parse (parser-of "foo") "foo") #f))

; ---------------------------------------------------------

(check (equal? (parse (string-parser) "") ""))
(check (equal? (parse (string-parser) "$a1") "$a1"))

; ---------------------------------------------------------

(check (equal? (parse (line-parser) "") #f))
(check (equal? (parse (line-parser) "\n") ""))
(check (equal? (parse (line-parser) "foo") #f))
(check (equal? (parse (line-parser) "foo\n") "foo"))

; ---------------------------------------------------------

(check (equal? (parse (positive-integer-parser) "") #f))
(check (equal? (parse (positive-integer-parser) "012") 12))
(check (equal? (parse (positive-integer-parser) "012a") #f))
(check (equal? (parse (positive-integer-parser) "-012") #f))

; ---------------------------------------------------------

(check (equal? (parse (word-parser) "") #f))
(check (equal? (parse (word-parser) "foo") `foo))
(check (equal? (parse (word-parser) "1") #f))
(check (equal? (parse (word-parser) "foo1") #f))

; ---------------------------------------------------------

(lets
  ($parser 
    (oneof-parser 
      (list 
        (word-parser)
        (positive-integer-parser))))
  (begin
    (check (equal? (parse $parser "") #f))
    (check (equal? (parse $parser "foo") `foo))
    (check (equal? (parse $parser "123") 123))
    (check (equal? (parse $parser "$1") #f))))

; ---------------------------------------------------------

(lets
  ($parser (indent-parser (string-parser)))
  (begin
    (check (equal? (parse $parser "") ""))
    (check (equal? (parse $parser " ") #f))
    (check (equal? (parse $parser "  ") #f))
    (check (equal? (parse $parser "  \n") "\n"))
    (check (equal? (parse $parser "  a") #f))
    (check (equal? (parse $parser "  a\n") "a\n"))))
