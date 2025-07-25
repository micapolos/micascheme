(import
  (rename (micascheme)
    (+ %+)
    (- %-)
    (string-append %string-append)
    (string-length %string-length)
    (bytevector %bytevector)
    (let* %let*))
  (asm typed)
  (asm block)
  (asm u)
  (asm binary)
  (asm core))

(define-syntax (check-typed $syntax $lookup)
  (syntax-case $syntax ()
    ((_ in out)
      #`(check
        (equal?
          '#,(datum->syntax #'+ (typed->datum (syntax->typed $lookup #'in)))
          'out)))))

(define-typed + (typed (function integer integer) %+))
(define-typed - (typed (function (integer . integer) integer) %-))
(define-typed string-append (typed (function string string) %string-append))
(define-typed string-length (typed (function (string) integer) %string-length))
(define-typed bytevector (typed (function integer bytevector) %bytevector))

(check-typed type (typed type type))

(check-typed void (typed type void))
(check-typed boolean (typed type boolean))
(check-typed integer (typed type integer))
(check-typed char (typed type char))
(check-typed string (typed type string))

(check-typed
  (function () boolean)
  (typed type (function () boolean)))

(check-typed
  (function (integer string) boolean)
  (typed type (function (integer string) boolean)))

(check-typed
  (function integer boolean)
  (typed type (function integer boolean)))

(check-typed
  (function (integer char . string) boolean)
  (typed type (function (integer char . string) boolean)))

(check-typed (void) (typed void (void)))
(check-typed #f (typed boolean #f))
(check-typed 123 (typed integer 123))
(check-typed #\a (typed char #\a))
(check-typed "foo" (typed string "foo"))

(check-typed + (typed (function integer integer) %+))
(check-typed string-append (typed (function string string) %string-append))
(check-typed string-length (typed (function (string) integer) %string-length))

(check-typed
  (lambda () "foo")
  (typed
    (function () string)
    (lambda () "foo")))

(check-typed
  (lambda ((i integer) (s string)) i)
  (typed
    (function (integer string) integer)
    (lambda (i s) i)))

(check-typed
  (lambda ((i integer) (s string)) s)
  (typed
    (function (integer string) string)
    (lambda (i s) s)))

(check-typed
  (lambda ((s string)) (string-length s))
  (typed
    (function (string) integer)
    (lambda (s) (%string-length s))))

(check-typed
  ((lambda ((s string)) (string-length s)) "foo")
  (typed
    integer
    ((lambda (s) (%string-length s)) "foo")))

(check-typed
  (let ((foo "foo") (bar "bar")) (string-append foo bar))
  (typed string (let ((foo "foo") (bar "bar")) (%string-append foo bar))))

(check-typed
  (let ((foo "foo") (bar "bar"))
    (let ((foo "foo2"))
      (string-append foo bar)))
  (typed string
    (let ((foo "foo") (bar "bar"))
      (let ((foo "foo2"))
        (%string-append foo bar)))))

(check-typed
  (let* ((foo "foo") (foobar (string-append foo "bar"))) foobar)
  (typed string
    (let ((foo "foo"))
      (let ((foobar (%string-append foo "bar")))
        foobar))))

(check-typed
  (string-append)
  (typed string (%string-append)))

(check-typed
  (string-append "a" "b" "c")
  (typed string (%string-append "a" "b" "c")))

(check-typed
  (bytevector 1 2 (+ 3 4))
  (typed bytevector (%bytevector 1 2 (%+ 3 4))))

(check-typed (u2 (+ 1 2)) (typed integer (u2 (%+ 1 2) (+ 1 2))))
(check-typed (u3 (+ 1 2)) (typed integer (u3 (%+ 1 2) (+ 1 2))))
(check-typed (u8 (+ 1 2)) (typed integer (u8 (%+ 1 2) (+ 1 2))))
(check-typed (u16 (+ 1 2)) (typed integer (u16 (%+ 1 2) (+ 1 2))))
(check-typed (s8 (+ 1 2)) (typed integer (s8 (%+ 1 2) (+ 1 2))))

(check-typed
  (zero-binary 123)
  (typed binary (zero-binary 123)))

(check-typed
  (db-binary (+ 1 2))
  (typed binary (db-binary (%+ 1 2) #'(+ 1 2))))

(check-typed
  (dw-binary (+ 1 2))
  (typed binary (dw-binary (%+ 1 2) #'(+ 1 2))))

(check-typed
  (binary-append (db-binary 1) (dw-binary 2))
  (typed binary
    (binary-append
      (db-binary 1 #'1)
      (dw-binary 2 #'2))))

(check-typed
  (binary->bytevector (db-binary 1))
  (typed bytevector (binary->bytevector (db-binary 1 #'1))))
