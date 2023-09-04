(import (micascheme) (leo parser))

(check (equal? (parse (parse-error) "") #f))
(check (equal? (parse (parse-error) "foo") #f))

(check (equal? (parse (parser "foo") "") "foo"))
(check (equal? (parse (parser "foo") "a") #f))
(check (equal? (parse (parser "foo") "foo") #f))

; ---------------------------------------------------------

(check (equal? (parse (char-parser) "") #f))
(check (equal? (parse (char-parser) "a") #\a))
(check (equal? (parse (char-parser) "1") #\1))
(check (equal? (parse (char-parser) "ab") #f))

; ---------------------------------------------------------

(check (equal? (parse (digit-parser) "") #f))
(check (equal? (parse (digit-parser) "0") 0))
(check (equal? (parse (digit-parser) "9") 9))
(check (equal? (parse (digit-parser) "10") #f))

; ---------------------------------------------------------

(check (equal? (parse (letter-parser) "") #f))
(check (equal? (parse (letter-parser) "a") #\a))
(check (equal? (parse (letter-parser) "1") #f))
(check (equal? (parse (letter-parser) "ab") #f))

; ---------------------------------------------------------

(check (equal? (parse (string-parser) "") ""))
(check (equal? (parse (string-parser) "$a1") "$a1"))

; ---------------------------------------------------------

(check (equal? (parse (exact-parser "") "") ""))
(check (equal? (parse (exact-parser "") "foo") #f))
(check (equal? (parse (exact-parser "foo") "fo") #f))
(check (equal? (parse (exact-parser "foo") "foo") "foo"))
(check (equal? (parse (exact-parser "foo") "foot") #f))

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
  ($bind-parser
    (parser-bind (positive-integer-parser)
      (lambda ($positive-integer)
        (parser-bind (word-parser)
          (lambda ($word)
            (parser 
              (string-append
                (symbol->string $word)
                ": "
                (number->string $positive-integer))))))))
  (begin
    (check (equal? (parse $bind-parser "") #f))
    (check (equal? (parse $bind-parser "123") #f))
    (check (equal? (parse $bind-parser "123m") "m: 123"))
    (check (equal? (parse $bind-parser "123cm") "cm: 123"))
    (check (equal? (parse $bind-parser "123!") #f))
    (check (equal? (parse $bind-parser "123cm!") #f))))

; ---------------------------------------------------------

(lets
  ($increment-parser
    (parser-lets 
      ($number (positive-integer-parser))
      (parser (+ $number 1))))
  (check (equal? (parse $increment-parser "12") 13)))

(lets
  ($starred-parser
    (parser-lets 
      ($skip (exact-parser "* "))
      (positive-integer-parser)))
  (check (equal? (parse $starred-parser "* 123") 123)))

(lets
  ($subtract-parser
    (parser-lets 
      ($number1 (positive-integer-parser))
      (skip (exact-parser " - "))
      ($number2 (positive-integer-parser))
      (parser (- $number1 $number2))))
  (check (equal? (parse $subtract-parser "3 - 2") 1)))

; ---------------------------------------------------------

(lets
  ($parser (make-parser "foo"))
  (check (equal? (parse $parser "foo") "foo")))

(lets
  ($parser (make-parser (pure (string-parser))))
  (check (equal? (parse $parser "foo") "foo")))

(lets
  ($parser (make-parser string))
  (check (equal? (parse $parser "foo") "foo")))

(lets
  ($parser (make-parser (stack char)))
  (check (equal? (parse $parser "foo") (stack #\f #\o #\o))))

(lets
  ($parser (make-parser (parsed 12.4)))
  (check (equal? (parse $parser "") 12.4)))

(lets
  ($parser 
    (make-parser 
      (lets
        ($number1 positive-integer)
        (skip "-")
        ($number2 positive-integer)
        (parsed (- $number1 $number2)))))
  (check (equal? (parse $parser "3-2") 1)))

; ---------------------------------------------------------

(begin
  (define-parser expr
    (lets
      ($number1 positive-integer)
      ($op
        (oneof 
          (lets (skip "+") (parsed +)) 
          (lets (skip "-") (parsed -))))
      ($number2 positive-integer)
      (parsed ($op $number1 $number2))))
  (begin
    (check (equal? (parse (make-parser expr) "3-2") 1))
    (check (equal? (parse (make-parser expr) "3+2") 5))))

; ---------------------------------------------------------

(lets
  ($map-parser (parser-map (line-parser) string-length))
  (begin
    (check (equal? (parse $map-parser "foo") #f))
    (check (equal? (parse $map-parser "foo\n") 3))
    (check (equal? (parse $map-parser "foo\nbar") #f))))

; ---------------------------------------------------------

(check 
  (equal? 
    (oneof-parser) 
    (parse-error)))

(check 
  (equal? 
    (oneof-parser 
      (parse-error)) 
    (parse-error)))

(check
  (equal? 
    (oneof-parser 
      (parse-error) 
      (parse-error)) 
    (parse-error)))

(lets
  ($oneof-parser 
    (oneof-parser 
      (parse-error) 
      (word-parser) 
      (positive-integer-parser)))
  (begin
    (check (equal? (parse $oneof-parser "") #f))
    (check (equal? (parse $oneof-parser "foo") `foo))
    (check (equal? (parse $oneof-parser "123") 123))
    (check (equal? (parse $oneof-parser "$1") #f))))

; ---------------------------------------------------------

(lets
  ($fold-parser 
    (fold-parser (stack) (line-parser) push))
  (begin
    (check (equal? (parse $fold-parser "") (stack)))
    (check (equal? (parse $fold-parser "foo") #f))
    (check (equal? (parse $fold-parser "foo\n") (stack "foo")))
    (check (equal? (parse $fold-parser "foo\nbar") #f))
    (check (equal? (parse $fold-parser "foo\nbar\n") (stack "foo" "bar")))))

; ---------------------------------------------------------

(lets
  ($stack-parser (stack-parser (line-parser)))
  (begin
    (check (equal? (parse $stack-parser "") (stack)))
    (check (equal? (parse $stack-parser "foo") #f))
    (check (equal? (parse $stack-parser "foo\n") (stack "foo")))
    (check (equal? (parse $stack-parser "foo\nbar") #f))
    (check (equal? (parse $stack-parser "foo\nbar\n") (stack "foo" "bar")))))

; ---------------------------------------------------------

(lets
  ($non-empty-stack-parser (non-empty-stack-parser (line-parser)))
  (begin
    (check (equal? (parse $non-empty-stack-parser "") #f))
    (check (equal? (parse $non-empty-stack-parser "foo") #f))
    (check (equal? (parse $non-empty-stack-parser "foo\n") (stack "foo")))
    (check (equal? (parse $non-empty-stack-parser "foo\nbar") #f))
    (check (equal? (parse $non-empty-stack-parser "foo\nbar\n") (stack "foo" "bar")))))

; ---------------------------------------------------------

(lets
  ($indent-parser (indent-parser (string-parser)))
  (begin
    (check (equal? (parse $indent-parser "") ""))
    (check (equal? (parse $indent-parser " ") #f))
    (check (equal? (parse $indent-parser "  ") #f))
    (check (equal? (parse $indent-parser "  \n") "\n"))
    (check (equal? (parse $indent-parser "  a") #f))
    (check (equal? (parse $indent-parser "  a\n") "a\n"))))

; ---------------------------------------------------------

(check (equal? (parse (string-literal-char-parser) "") #f))
(check (equal? (parse (string-literal-char-parser) "a") #\a))
(check (equal? (parse (string-literal-char-parser) "\\") #f))
(check (equal? (parse (string-literal-char-parser) "\"") #f))
(check (equal? (parse (string-literal-char-parser) "\\n") #\newline))
(check (equal? (parse (string-literal-char-parser) "\\t") #\tab))
(check (equal? (parse (string-literal-char-parser) "\\\\") #\\))
(check (equal? (parse (string-literal-char-parser) "ab") #f))

; ---------------------------------------------------------

(check (equal? (parse (literal-string-parser) "") #f))
(check (equal? (parse (literal-string-parser) "\"") #f))
(check (equal? (parse (literal-string-parser) "\"\"") ""))
(check (equal? (parse (literal-string-parser) "\"\\\"\"") "\""))
(check (equal? (parse (literal-string-parser) "\"foo\"") "foo"))
