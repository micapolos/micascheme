(import (micascheme) (leo parser))

; ---------------------------------------------------------

(check (obj=? (parse #f "") #f))
(check (obj=? (parse #f "foo") #f))

(check (obj=? (parse (parser "foo") "") "foo"))
(check (obj=? (parse (parser "foo") "a") (parse-error 1 1)))
(check (obj=? (parse (parser "foo") "foo") (parse-error 1 1)))

; ---------------------------------------------------------

(check (obj=? (parse (exact-parser "") "") ""))
(check (obj=? (parse (exact-parser "") "foo") (parse-error 1 1)))
(check (obj=? (parse (exact-parser "foo") "fo") (parse-error 1 3)))
(check (obj=? (parse (exact-parser "foo") "foo") "foo"))
(check (obj=? (parse (exact-parser "foo") "foot") (parse-error 1 4)))

; ---------------------------------------------------------

(lets
  ($bind-parser
    (parser-bind (exact-parser "fo")
      (lambda (_)
        (parser-bind (exact-parser "12")
          (lambda (_)
            (parser #t))))))
  (begin
    (check (obj=? (parse $bind-parser "") (parse-error 1 1)))
    (check (obj=? (parse $bind-parser "a") (parse-error 1 1)))
    (check (obj=? (parse $bind-parser "f") (parse-error 1 2)))
    (check (obj=? (parse $bind-parser "fa") (parse-error 1 2)))
    (check (obj=? (parse $bind-parser "fo") (parse-error 1 3)))
    (check (obj=? (parse $bind-parser "fo0") (parse-error 1 3)))
    (check (obj=? (parse $bind-parser "fo1") (parse-error 1 4)))
    (check (obj=? (parse $bind-parser "fo12") #t))
    (check (obj=? (parse $bind-parser "fo10") (parse-error 1 4)))
    (check (obj=? (parse $bind-parser "fo123") (parse-error 1 5)))))

; ---------------------------------------------------------

(check (obj=? (oneof-parser) #f))
(check (obj=? (oneof-parser #f) #f))
(check (obj=? (oneof-parser #f #f) #f))

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
    (check (obj=? (parse $oneof-parser "") ""))
    (check (obj=? (parse $oneof-parser "a") "a"))
    (check (obj=? (parse $oneof-parser "b") "b"))
    (check (obj=? (parse $oneof-parser "c") (parse-error 1 2)))
    (check (obj=? (parse $oneof-parser "d") (parse-error 1 1)))
    (check (obj=? (parse $oneof-parser "aa") "aa"))
    (check (obj=? (parse $oneof-parser "ab") "ab"))
    (check (obj=? (parse $oneof-parser "ac") (parse-error 1 3)))
    (check (obj=? (parse $oneof-parser "ad") (parse-error 1 2)))))

; ---------------------------------------------------------

(check (obj=? (parse (char-parser) "") (parse-error 1 1)))
(check (obj=? (parse (char-parser) "a") #\a))
(check (obj=? (parse (char-parser) "1") #\1))
(check (obj=? (parse (char-parser) "ab") (parse-error 1 2)))

; ---------------------------------------------------------

(check (obj=? (parse (digit-parser) "") (parse-error 1 1)))
(check (obj=? (parse (digit-parser) "0") 0))
(check (obj=? (parse (digit-parser) "9") 9))
(check (obj=? (parse (digit-parser) "10") (parse-error 1 2)))

; ---------------------------------------------------------

(check (obj=? (parse (letter-parser) "") (parse-error 1 1)))
(check (obj=? (parse (letter-parser) "a") #\a))
(check (obj=? (parse (letter-parser) "1") (parse-error 1 2))) ; wrong!!!
(check (obj=? (parse (letter-parser) "ab") (parse-error 1 2)))

; ---------------------------------------------------------

(check (obj=? (parse (string-parser) "") ""))
(check (obj=? (parse (string-parser) "$a1") "$a1"))

; ---------------------------------------------------------

(check (obj=? (parse (line-parser) "") (parse-error 1 1)))
(check (obj=? (parse (line-parser) "\n") ""))
(check (obj=? (parse (line-parser) "foo") (parse-error 1 4)))
(check (obj=? (parse (line-parser) "foo\n") "foo"))

; ---------------------------------------------------------

(check (obj=? (parse (positive-integer-parser) "") (parse-error 1 1)))
(check (obj=? (parse (positive-integer-parser) "012") 12))
(check (obj=? (parse (positive-integer-parser) "012a") (parse-error 1 5))) ; wrong!!!
(check (obj=? (parse (positive-integer-parser) "-012") (parse-error 1 2))) ; wrong!!!

; ---------------------------------------------------------

(check (obj=? (parse (word-parser) "") (parse-error 1 1)))
(check (obj=? (parse (word-parser) "foo") `foo))
(check (obj=? (parse (word-parser) "1") (parse-error 1 2))) ; wrong!!!
(check (obj=? (parse (word-parser) "foo1") (parse-error 1 5))) ; wrong!!!

; ---------------------------------------------------------

(lets
  ($bind-parser-2
    (parser-bind (positive-integer-parser)
      (lambda ($number)
        (parser-bind (word-parser)
          (lambda ($unit)
            (parser (cons $number $unit)))))))
  (begin
    (check (obj=? (parse $bind-parser-2 "") (parse-error 1 1)))
    (check (obj=? (parse $bind-parser-2 "123") (parse-error 1 4)))
    (check (obj=? (parse $bind-parser-2 "123m") (cons 123 `m)))
    (check (obj=? (parse $bind-parser-2 "123cm") (cons 123 `cm)))
    (check (obj=? (parse $bind-parser-2 "123!") (parse-error 1 5))) ; wrong!!!
    (check (obj=? (parse $bind-parser-2 "123cm!") (parse-error 1 5))))) ; wrong!!!

; ---------------------------------------------------------

(lets
  ($increment-parser
    (parser-lets 
      ($number (positive-integer-parser))
      (parser (+ $number 1))))
  (check (obj=? (parse $increment-parser "12") 13)))

(lets
  ($starred-parser
    (parser-lets 
      ($skip (exact-parser "* "))
      (positive-integer-parser)))
  (check (obj=? (parse $starred-parser "* 123") 123)))

(lets
  ($subtract-parser
    (parser-lets 
      ($number1 (positive-integer-parser))
      (skip (exact-parser " - "))
      ($number2 (positive-integer-parser))
      (parser (- $number1 $number2))))
  (check (obj=? (parse $subtract-parser "3 - 2") 1)))

; ---------------------------------------------------------

(lets
  ($parser (make-parser "foo"))
  (check (obj=? (parse $parser "foo") "foo")))

(lets
  ($parser (make-parser (pure (string-parser))))
  (check (obj=? (parse $parser "foo") "foo")))

(lets
  ($parser (make-parser string))
  (check (obj=? (parse $parser "foo") "foo")))

(lets
  ($parser (make-parser (stack char)))
  (check (obj=? (parse $parser "foo") (stack #\f #\o #\o))))

(lets
  ($parser (make-parser (parsed 12.4)))
  (check (obj=? (parse $parser "") 12.4)))

(lets
  ($parser 
    (make-parser 
      (lets
        ($number1 positive-integer)
        (skip "-")
        ($number2 positive-integer)
        (parsed (- $number1 $number2)))))
  (check (obj=? (parse $parser "3-2") 1)))

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
    (check (obj=? (parse (make-parser expr) "3-2") 1))
    (check (obj=? (parse (make-parser expr) "3+2") 5))))

; ---------------------------------------------------------

(lets
  ($map-parser (parser-map (line-parser) string-length))
  (begin
    (check (obj=? (parse $map-parser "foo") #f))
    (check (obj=? (parse $map-parser "foo\n") 3))
    (check (obj=? (parse $map-parser "foo\nbar") #f))))

; ---------------------------------------------------------

(check (obj=? (oneof-parser) #f))
(check (obj=? (oneof-parser #f) #f))
(check (obj=? (oneof-parser #f #f) #f))

(lets
  ($oneof-parser 
    (oneof-parser 
      #f 
      (word-parser) 
      (positive-integer-parser)))
  (begin
    (check (obj=? (parse $oneof-parser "") #f))
    (check (obj=? (parse $oneof-parser "foo") `foo))
    (check (obj=? (parse $oneof-parser "123") 123))
    (check (obj=? (parse $oneof-parser "$1") #f))))

; ---------------------------------------------------------

(lets
  ($fold-parser 
    (fold-parser (stack) (line-parser) push))
  (begin
    (check (obj=? (parse $fold-parser "") (stack)))
    (check (obj=? (parse $fold-parser "foo") #f))
    (check (obj=? (parse $fold-parser "foo\n") (stack "foo")))
    (check (obj=? (parse $fold-parser "foo\nbar") #f))
    (check (obj=? (parse $fold-parser "foo\nbar\n") (stack "foo" "bar")))))

; ---------------------------------------------------------

(lets
  ($stack-parser (stack-parser (line-parser)))
  (begin
    (check (obj=? (parse $stack-parser "") (stack)))
    (check (obj=? (parse $stack-parser "foo") #f))
    (check (obj=? (parse $stack-parser "foo\n") (stack "foo")))
    (check (obj=? (parse $stack-parser "foo\nbar") #f))
    (check (obj=? (parse $stack-parser "foo\nbar\n") (stack "foo" "bar")))))

; ---------------------------------------------------------

(lets
  ($non-empty-stack-parser (non-empty-stack-parser (line-parser)))
  (begin
    (check (obj=? (parse $non-empty-stack-parser "") #f))
    (check (obj=? (parse $non-empty-stack-parser "foo") #f))
    (check (obj=? (parse $non-empty-stack-parser "foo\n") (stack "foo")))
    (check (obj=? (parse $non-empty-stack-parser "foo\nbar") #f))
    (check (obj=? (parse $non-empty-stack-parser "foo\nbar\n") (stack "foo" "bar")))))

; ---------------------------------------------------------

(lets
  ($indent-parser (indent-parser (string-parser)))
  (begin
    (check (obj=? (parse $indent-parser "") ""))
    (check (obj=? (parse $indent-parser " ") #f))
    (check (obj=? (parse $indent-parser "  ") #f))
    (check (obj=? (parse $indent-parser "  \n") "\n"))
    (check (obj=? (parse $indent-parser "  a") #f))
    (check (obj=? (parse $indent-parser "  a\n") "a\n"))))

; ---------------------------------------------------------

(check (obj=? (parse (string-literal-char-parser) "") #f))
(check (obj=? (parse (string-literal-char-parser) "a") #\a))
(check (obj=? (parse (string-literal-char-parser) "\\") #f))
(check (obj=? (parse (string-literal-char-parser) "\"") #f))
(check (obj=? (parse (string-literal-char-parser) "\\n") #\newline))
(check (obj=? (parse (string-literal-char-parser) "\\t") #\tab))
(check (obj=? (parse (string-literal-char-parser) "\\\\") #\\))
(check (obj=? (parse (string-literal-char-parser) "ab") #f))

; ---------------------------------------------------------

(check (obj=? (parse (literal-string-parser) "") #f))
(check (obj=? (parse (literal-string-parser) "\"") #f))
(check (obj=? (parse (literal-string-parser) "\"\"") ""))
(check (obj=? (parse (literal-string-parser) "\"\\\"\"") "\""))
(check (obj=? (parse (literal-string-parser) "\"foo\"") "foo"))
