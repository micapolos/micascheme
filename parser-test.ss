(import (micascheme) (parser))

; ---------------------------------------------------------

(check (equal? (parse #f "") #f))
(check (equal? (parse #f "foo") #f))

(check (equal? (parse (parser "foo") "") "foo"))
(check (equal? (parse (parser "foo") "a") (parse-error 1 1)))
(check (equal? (parse (parser "foo") "foo") (parse-error 1 1)))

; ---------------------------------------------------------

(lets
  ($char-parser-bind
    (char-parser-bind $char
      (and (char-numeric? $char)
        (parser (string $char)))))
  (begin
    (check (equal? (parse $char-parser-bind "") (parse-error 1 1)))
    (check (equal? (parse $char-parser-bind "1") "1"))
    (check (equal? (parse $char-parser-bind "12") (parse-error 1 2)))
    (check (equal? (parse $char-parser-bind "a") (parse-error 1 1)))))

; ---------------------------------------------------------

(check (equal? (parse (char-parser) "") (parse-error 1 1)))
(check (equal? (parse (char-parser) "a") #\a))
(check (equal? (parse (char-parser) "1") #\1))
(check (equal? (parse (char-parser) "ab") (parse-error 1 2)))

; ---------------------------------------------------------

(lets
  ($exact-char-parser (exact-char-parser #\a))
  (begin
    (check (equal? (parse $exact-char-parser "") (parse-error 1 1)))
    (check (equal? (parse $exact-char-parser "a") #\a))
    (check (equal? (parse $exact-char-parser "ab") (parse-error 1 2)))
    (check (equal? (parse $exact-char-parser "b") (parse-error 1 1)))))

; ---------------------------------------------------------

(check (equal? (parse (exact-parser "") "") ""))
(check (equal? (parse (exact-parser "") "foo") (parse-error 1 1)))
(check (equal? (parse (exact-parser "foo") "fo") (parse-error 1 3)))
(check (equal? (parse (exact-parser "foo") "foo") "foo"))
(check (equal? (parse (exact-parser "foo") "foot") (parse-error 1 4)))

; ---------------------------------------------------------

(lets
  ($bind-parser
    (parser-bind (exact-parser "fo")
      (lambda (_)
        (parser-bind (exact-parser "12")
          (lambda (_)
            (parser #t))))))
  (begin
    (check (equal? (parse $bind-parser "") (parse-error 1 1)))
    (check (equal? (parse $bind-parser "a") (parse-error 1 1)))
    (check (equal? (parse $bind-parser "f") (parse-error 1 2)))
    (check (equal? (parse $bind-parser "fa") (parse-error 1 2)))
    (check (equal? (parse $bind-parser "fo") (parse-error 1 3)))
    (check (equal? (parse $bind-parser "fo0") (parse-error 1 3)))
    (check (equal? (parse $bind-parser "fo1") (parse-error 1 4)))
    (check (equal? (parse $bind-parser "fo12") #t))
    (check (equal? (parse $bind-parser "fo10") (parse-error 1 4)))
    (check (equal? (parse $bind-parser "fo123") (parse-error 1 5)))))

; ---------------------------------------------------------

(check (equal? (oneof-parser) #f))
(check (equal? (oneof-parser #f) #f))
(check (equal? (oneof-parser #f #f) #f))

(lets
  ($oneof-parser 
    (oneof-parser 
      #f
      (exact-parser "")
      (exact-parser "a")
      (exact-parser "b")
      (exact-parser "c")
      (exact-parser "c")
      (exact-parser "aa")
      (exact-parser "ab")
      (exact-parser "ac")
      (exact-parser "ac")))
  (begin
    (check (equal? (parse $oneof-parser "") ""))
    (check (equal? (parse $oneof-parser "a") "a"))
    (check (equal? (parse $oneof-parser "b") "b"))
    (check (equal? (parse $oneof-parser "c") (parse-error 1 2)))
    (check (equal? (parse $oneof-parser "d") (parse-error 1 1)))
    (check (equal? (parse $oneof-parser "aa") "aa"))
    (check (equal? (parse $oneof-parser "ab") "ab"))
    (check (equal? (parse $oneof-parser "ac") (parse-error 1 3)))
    (check (equal? (parse $oneof-parser "ad") (parse-error 1 2)))
    (check (equal? (parse $oneof-parser "acd") (parse-error 1 3)))))

(lets
  ($oneof-parser 
    (oneof-parser 
      (exact-parser "a")
      (exact-parser "b")
      (else (char-parser))))
  (begin
    (check (equal? (parse $oneof-parser "") (parse-error 1 1)))
    (check (equal? (parse $oneof-parser "a") "a"))
    (check (equal? (parse $oneof-parser "b") "b"))
    (check (equal? (parse $oneof-parser "c") #\c))))

; ---------------------------------------------------------

(lets
  ($opt-parser (opt-parser (exact-parser "foo")))
  (begin
    (check (equal? (parse $opt-parser "") #f))
    (check (equal? (parse $opt-parser "fo") (parse-error 1 3)))
    (check (equal? (parse $opt-parser "foo") "foo"))
    (check (equal? (parse $opt-parser "fooo") (parse-error 1 4)))))

; ---------------------------------------------------------

(lets
  ($fold-parser 
    (fold-parser (stack) (newline-ended-parser (positive-integer-parser)) push))
  (begin
    (check (equal? (parse $fold-parser "") (stack)))
    (check (equal? (parse $fold-parser "12") (parse-error 1 3)))
    (check (equal? (parse $fold-parser "12\n") (stack 12)))
    (check (equal? (parse $fold-parser "12\n34") (parse-error 2 3)))
    (check (equal? (parse $fold-parser "12\n34\n") (stack 12 34)))))

; ---------------------------------------------------------

(lets
  ($parser-until-newline (parser-until-newline (string-parser)))
  (begin
    (check (equal? (parse $parser-until-newline "") ""))
    (check (equal? (parse $parser-until-newline "abc") "abc"))
    (check (equal? (parse $parser-until-newline "abc\n") (parse-error 1 4)))))

; ---------------------------------------------------------

(lets
  ($stack-parser (stack-parser (newline-ended-parser (positive-integer-parser))))
  (begin
    (check (equal? (parse $stack-parser "") (stack)))
    (check (equal? (parse $stack-parser "12") (parse-error 1 3)))
    (check (equal? (parse $stack-parser "12\n") (stack 12)))
    (check (equal? (parse $stack-parser "12\n34") (parse-error 2 3)))
    (check (equal? (parse $stack-parser "12\n34\n") (stack 12 34)))))

; ---------------------------------------------------------

(lets
  ($separated-stack-parser
    (separated-stack-parser
      (positive-integer-parser) 
      (exact-parser ",")))
  (begin
    (check (equal? (parse $separated-stack-parser "") (stack)))
    (check (equal? (parse $separated-stack-parser "1") (stack 1)))
    (check (equal? (parse $separated-stack-parser "12") (stack 12)))
    (check (equal? (parse $separated-stack-parser "12,3") (stack 12 3)))
    (check (equal? (parse $separated-stack-parser "12,34") (stack 12 34)))
    (check (equal? (parse $separated-stack-parser "12,34,5") (stack 12 34 5)))
    (check (equal? (parse $separated-stack-parser "12,34,56") (stack 12 34 56)))))

; ---------------------------------------------------------

(lets
  ($non-empty-stack-parser 
    (non-empty-stack-parser 
      (newline-ended-parser 
        (positive-integer-parser))))
  (begin
    (check (equal? (parse $non-empty-stack-parser "") (parse-error 1 1)))
    (check (equal? (parse $non-empty-stack-parser "12") (parse-error 1 3)))
    (check (equal? (parse $non-empty-stack-parser "12\n") (stack 12)))
    (check (equal? (parse $non-empty-stack-parser "12\n34") (parse-error 2 3)))
    (check (equal? (parse $non-empty-stack-parser "12\n34\n") (stack 12 34)))))

; ---------------------------------------------------------

(lets
  ($non-empty-separated-stack-parser
    (non-empty-separated-stack-parser
      (positive-integer-parser)
      (exact-parser ",")))
  (begin
    (check (equal? (parse $non-empty-separated-stack-parser "") (parse-error 1 1)))
    (check (equal? (parse $non-empty-separated-stack-parser "1") (stack 1)))
    (check (equal? (parse $non-empty-separated-stack-parser "12") (stack 12)))
    (check (equal? (parse $non-empty-separated-stack-parser "12,3") (stack 12 3)))
    (check (equal? (parse $non-empty-separated-stack-parser "12,34") (stack 12 34)))
    (check (equal? (parse $non-empty-separated-stack-parser "12,34,5") (stack 12 34 5)))
    (check (equal? (parse $non-empty-separated-stack-parser "12,34,56") (stack 12 34 56)))))

; ---------------------------------------------------------

(check (equal? (parse (digit-parser) "") (parse-error 1 1)))
(check (equal? (parse (digit-parser) "0") 0))
(check (equal? (parse (digit-parser) "9") 9))
(check (equal? (parse (digit-parser) "a") (parse-error 1 1)))
(check (equal? (parse (digit-parser) "10") (parse-error 1 2)))

; ---------------------------------------------------------

(check (equal? (parse (letter-parser) "") (parse-error 1 1)))
(check (equal? (parse (letter-parser) "a") #\a))
(check (equal? (parse (letter-parser) "1") (parse-error 1 1)))
(check (equal? (parse (letter-parser) "ab") (parse-error 1 2)))

; ---------------------------------------------------------

(check (equal? (parse (string-parser) "") ""))
(check (equal? (parse (string-parser) "$a1") "$a1"))

; ---------------------------------------------------------

(check (equal? (parse (positive-integer-parser) "") (parse-error 1 1)))
(check (equal? (parse (positive-integer-parser) "012") 12))
(check (equal? (parse (positive-integer-parser) "012a") (parse-error 1 4)))
(check (equal? (parse (positive-integer-parser) "-012") (parse-error 1 1)))

; ---------------------------------------------------------

(check (equal? (parse (integer-parser) "") (parse-error 1 1)))
(check (equal? (parse (integer-parser) "012") 12))
(check (equal? (parse (integer-parser) "012a") (parse-error 1 4)))
(check (equal? (parse (integer-parser) "-012") -12))
(check (equal? (parse (integer-parser) "+012") 12))
(check (equal? (parse (integer-parser) "-") (parse-error 1 2)))
(check (equal? (parse (integer-parser) "+") (parse-error 1 2)))

; ---------------------------------------------------------

(check (equal? (parse (word-parser) "") (parse-error 1 1)))
(check (equal? (parse (word-parser) "foo") `foo))
(check (equal? (parse (word-parser) "1") (parse-error 1 1)))
(check (equal? (parse (word-parser) "foo1") (parse-error 1 4)))

; ---------------------------------------------------------

(lets
  ($bind-parser-2
    (parser-bind (positive-integer-parser)
      (lambda ($number)
        (parser-bind (word-parser)
          (lambda ($unit)
            (parser (cons $number $unit)))))))
  (begin
    (check (equal? (parse $bind-parser-2 "") (parse-error 1 1)))
    (check (equal? (parse $bind-parser-2 "123") (parse-error 1 4)))
    (check (equal? (parse $bind-parser-2 "123m") (cons 123 `m)))
    (check (equal? (parse $bind-parser-2 "123cm") (cons 123 `cm)))
    (check (equal? (parse $bind-parser-2 "123!") (parse-error 1 4)))
    (check (equal? (parse $bind-parser-2 "123cm!") (parse-error 1 6)))))

; ---------------------------------------------------------

(lets
  ($increment-parser
    (lets
      ((parser $number) (positive-integer-parser))
      (parser (+ $number 1))))
  (check (equal? (parse $increment-parser "12") 13)))

(lets
  ($starred-parser
    (lets
      ((parser _) (exact-parser "* "))
      (positive-integer-parser)))
  (check (equal? (parse $starred-parser "* 123") 123)))

(lets
  ($subtract-parser
    (lets
      ((parser $number1) (positive-integer-parser))
      ((parser _) (exact-parser " - "))
      ((parser $number2) (positive-integer-parser))
      (parser (- $number1 $number2))))
  (check (equal? (parse $subtract-parser "3 - 2") 1)))

; ---------------------------------------------------------

(check (equal? (oneof-parser) #f))
(check (equal? (oneof-parser #f) #f))
(check (equal? (oneof-parser #f #f) #f))

(lets
  ($oneof-parser 
    (oneof-parser 
      #f 
      (word-parser) 
      (positive-integer-parser)))
  (begin
    (check (equal? (parse $oneof-parser "") (parse-error 1 1)))
    (check (equal? (parse $oneof-parser "foo") `foo))
    (check (equal? (parse $oneof-parser "123") 123))
    (check (equal? (parse $oneof-parser "$1") (parse-error 1 1)))))

; ---------------------------------------------------------

(lets
  ($indent-parser (indent-parser (string-parser)))
  (begin
    (check (equal? (parse $indent-parser "") (parse-error 1 1)))
    (check (equal? (parse $indent-parser " ") (parse-error 1 2)))
    (check (equal? (parse $indent-parser "  ") ""))
    (check (equal? (parse $indent-parser "  a") "a"))
    (check (equal? (parse $indent-parser "  ab") "ab"))
    (check (equal? (parse $indent-parser "  ab\n") (parse-error 2 1)))
    (check (equal? (parse $indent-parser "  ab\n ") (parse-error 2 2)))
    (check (equal? (parse $indent-parser "  ab\n  ") "ab\n"))
    (check (equal? (parse $indent-parser "  ab\n  c") "ab\nc"))))

(lets
  ($skip-empty-lines-parser (skip-empty-lines-parser (string-parser)))
  (begin
    (check (equal? (parse $skip-empty-lines-parser "") ""))
    (check (equal? (parse $skip-empty-lines-parser "ab") "ab"))
    (check (equal? (parse $skip-empty-lines-parser "\n") ""))
    (check (equal? (parse $skip-empty-lines-parser "\n\n") ""))
    (check (equal? (parse $skip-empty-lines-parser "\n\nab") "ab"))
    (check (equal? (parse $skip-empty-lines-parser "ab\ncd") "ab\ncd"))
    (check (equal? (parse $skip-empty-lines-parser "ab\n\ncd") "ab\ncd"))
    (check (equal? (parse $skip-empty-lines-parser "ab\n\n\ncd") "ab\ncd"))
    (check (equal? (parse $skip-empty-lines-parser "ab\n\n") "ab\n"))
    (check (equal? (parse $skip-empty-lines-parser "ab\n\n\n") "ab\n"))))

; ---------------------------------------------------------

(check (equal? (parse (string-literal-char-parser) "") (parse-error 1 1)))
(check (equal? (parse (string-literal-char-parser) "a") #\a))
(check (equal? (parse (string-literal-char-parser) "\\") (parse-error 1 2)))
(check (equal? (parse (string-literal-char-parser) "\"") (parse-error 1 1)))
(check (equal? (parse (string-literal-char-parser) "\\n") #\newline))
(check (equal? (parse (string-literal-char-parser) "\\t") #\tab))
(check (equal? (parse (string-literal-char-parser) "\\\\") #\\))
(check (equal? (parse (string-literal-char-parser) "ab") (parse-error 1 2)))

; ---------------------------------------------------------

(check (equal? (parse (literal-string-parser) "") (parse-error 1 1)))
(check (equal? (parse (literal-string-parser) "\"") (parse-error 1 2)))
(check (equal? (parse (literal-string-parser) "\"\"") ""))
(check (equal? (parse (literal-string-parser) "\"\\\"\"") "\""))
(check (equal? (parse (literal-string-parser) "\"foo\"") "foo"))

; ---------------------------------------------------------

(check (equal? (parse-port (string-parser) (open-input-string "$a1")) "$a1"))
