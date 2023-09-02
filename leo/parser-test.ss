(import (micascheme) (leo parser))

(define (string-push $string $char)
  (string-append $string (string $char)))

(define (word-push $string $char)
  (and
    (char-alphabetic? $char)
    (string-append $string (string $char))))

(define (number-push $string $char)
  (and
    (char-numeric? $char)
    (string-append $string (string $char))))

; ---------------------------------------------------------

(check (equal? (parser-process (parser-of "foo") "") "foo"))
(check (equal? (parser-process (parser-of "foo") "a") #f))
(check (equal? (parser-process (parser-of "foo") "foo") #f))

; ---------------------------------------------------------

(check (equal? (parser-process (string-parser) "") ""))
(check (equal? (parser-process (string-parser) "$a1") "$a1"))

; ---------------------------------------------------------

(check (equal? (parser-process (line-parser) "") #f))
(check (equal? (parser-process (line-parser) "\n") ""))
(check (equal? (parser-process (line-parser) "foo") #f))
(check (equal? (parser-process (line-parser) "foo\n") "foo"))

; ---------------------------------------------------------

(check (equal? (parser-process (positive-integer-parser) "") #f))
(check (equal? (parser-process (positive-integer-parser) "012") 12))
(check (equal? (parser-process (positive-integer-parser) "012a") #f))
(check (equal? (parser-process (positive-integer-parser) "-012") #f))

; ---------------------------------------------------------

(check (equal? (parser-process (word-parser) "") #f))
(check (equal? (parser-process (word-parser) "foo") `foo))
(check (equal? (parser-process (word-parser) "1") #f))
(check (equal? (parser-process (word-parser) "foo1") #f))

; ---------------------------------------------------------

(lets
  ($parser 
    (oneof-parser 
      (list 
        (word-parser)
        (positive-integer-parser))))
  (begin
    (check (equal? (parser-process $parser "") #f))
    (check (equal? (parser-process $parser "foo") `foo))
    (check (equal? (parser-process $parser "123") 123))
    (check (equal? (parser-process $parser "$1") #f))))

; ---------------------------------------------------------

(lets
  ($parser (indent-parser (string-parser)))
  (begin
    (check (equal? (parser-process $parser "") ""))
    (check (equal? (parser-process $parser " ") #f))
    (check (equal? (parser-process $parser "  ") #f))
    (check (equal? (parser-process $parser "  \n") "\n"))
    (check (equal? (parser-process $parser "  a") #f))
    ;(check (equal? (parser-process $parser "  a\n") "a\n"))
    ))
