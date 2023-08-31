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

(check (equal? (processor-process (string-processor) "") ""))
(check (equal? (processor-process (string-processor) "$a1") "$a1"))

; ---------------------------------------------------------

(check (equal? (processor-process (positive-integer-processor) "") #f))
(check (equal? (processor-process (positive-integer-processor) "012") 12))
(check (equal? (processor-process (positive-integer-processor) "012a") #f))
(check (equal? (processor-process (positive-integer-processor) "-012") #f))

; ---------------------------------------------------------

(check (equal? (processor-process (word-processor) "") #f))
(check (equal? (processor-process (word-processor) "foo") `foo))
(check (equal? (processor-process (word-processor) "1") #f))
(check (equal? (processor-process (word-processor) "foo1") #f))

; ---------------------------------------------------------

(lets
  ($processor 
    (oneof-processor 
      (list 
        (word-processor)
        (positive-integer-processor))))
  (begin
    (check (equal? (processor-process $processor "") #f))
    (check (equal? (processor-process $processor "foo") `foo))
    (check (equal? (processor-process $processor "123") 123))
    (check (equal? (processor-process $processor "$1") #f))))

; ---------------------------------------------------------

(lets
  ($processor (indented-processor (string-processor)))
  (begin
    (check (equal? (processor-process $processor "") ""))
    (check (equal? (processor-process $processor " ") #f))
    (check (equal? (processor-process $processor "  ") #f))
    (check (equal? (processor-process $processor "  \n") "\n"))
    (check (equal? (processor-process $processor "  a") #f))
    ;(check (equal? (processor-process $processor "  a\n") "a\n"))
    ))

; ---------------------------------------------------------

(check
  (obj=?
    (empty-string-parser)
    (string-parser (stack))))

(check
  (obj=?
    (string-parser-push (string-parser (stack #\a #\space)) #\1)
    (string-parser (stack #\a #\space #\1))))

(check
  (obj=?
    (string-parser-finish (empty-string-parser))
    ""))

(check
  (obj=?
    (string-parser-finish (string-parser (stack #\a #\space #\1)))
    "a 1"))

; ---------------------------------------------------------

(check
  (obj=?
    (empty-positive-integer-parser)
    (positive-integer-parser (stack))))

(check
  (obj=?
    (positive-integer-parser-push
      (positive-integer-parser (stack 0 1 2))
      #\3)
    (positive-integer-parser (stack 0 1 2 3))))

(check
  (obj=?
    (positive-integer-parser-push
      (positive-integer-parser (stack 0 1 2))
      #\a)
    #f))

(check
  (obj=?
    (positive-integer-parser-finish 
      (positive-integer-parser (stack)))
    #f))

(check
  (obj=?
    (positive-integer-parser-finish
      (positive-integer-parser (stack 0 1 2)))
    12))

; ---------------------------------------------------------

(check
  (obj=?
    (empty-word-parser)
    (word-parser (stack))))

(check
  (obj=?
    (word-parser-push (word-parser (stack #\a #\b)) #\c)
    (word-parser (stack #\a #\b #\c))))

(check
  (obj=?
    (word-parser-push (word-parser (stack #\a #\b)) #\1)
    #f))

(check
  (obj=?
    (word-parser-finish (empty-word-parser))
    #f))

(check
  (obj=?
    (word-parser-finish (word-parser (stack #\a #\b)))
    `ab))

; ---------------------------------------------------------

(check
  (obj=?
    (alternatives-parser-push
      (list string-push word-push number-push)
      (alternatives-parser (list #f "foo" "123"))
      #\a)
    (alternatives-parser (list #f "fooa" #f))))

(check
  (obj=?
    (alternatives-parser-push
      (list string-push word-push number-push)
      (alternatives-parser (list #f "foo" "123"))
      #\4)
    (alternatives-parser (list #f #f "1234"))))

(check
  (obj=?
    (alternatives-parser-push
      (list string-push word-push number-push)
      (alternatives-parser (list #f "foo" "123"))
      #\space)
    (alternatives-parser (list #f #f #f))))

(check
  (obj=?
    (alternatives-parser-finish
      (list identity identity identity)
      (alternatives-parser (list #f "foo" "123")))
    #f))

(check
  (obj=?
    (alternatives-parser-finish
      (list identity identity identity)
      (alternatives-parser (list #f "foo" #f)))
    "foo"))

(check
  (obj=?
    (alternatives-parser-finish
      (list identity identity identity)
      (alternatives-parser (list #f #f #f)))
    #f))

; ---------------------------------------------------------

(check
  (obj=?
    (empty-indented-parser "foo")
    (indented-parser 0 "foo")))

(check
  (obj=?
    (indented-parser-push string-push (indented-parser 0 "foo") #\space)
    (indented-parser 1 "foo")))

(check
  (obj=?
    (indented-parser-push string-push (indented-parser 1 "foo") #\space)
    (indented-parser 2 "foo")))

(check
  (obj=?
    (indented-parser-push string-push (indented-parser 2 "foo") #\space)
    (indented-parser 2 "foo ")))

(check
  (obj=?
    (indented-parser-push string-push (indented-parser 0 "foo") #\newline)
    (indented-parser 0 "foo\n")))

(check
  (obj=?
    (indented-parser-push string-push (indented-parser 1 "foo") #\newline)
    #f))

(check
  (obj=?
    (indented-parser-push string-push (indented-parser 2 "foo") #\newline)
    (indented-parser 0 "foo\n")))

(check
  (obj=?
    (indented-parser-push string-push (indented-parser 0 "foo") #\a)
    #f))

(check
  (obj=?
    (indented-parser-push string-push (indented-parser 1 "foo") #\a)
    #f))

(check
  (obj=?
    (indented-parser-push string-push (indented-parser 2 "foo") #\a)
    (indented-parser 3 "fooa")))

; ---------------------------------------------------------

