(import
  (rename (micascheme)
    (+ %+)
    (string-append %string-append)
    (string-length %string-length))
  (asm-2 typed))

(define-syntax (check-typed $syntax $lookup)
  (syntax-case $syntax ()
    ((_ in out)
      #`(check
        (equal?
          '#,(syntax->typed $lookup #'in)
          'out)))))

(define-typed + (typed (procedure integer integer) %+))
(define-typed string-append (typed (procedure string string) %string-append))
(define-typed string-length (typed (procedure (string) integer) %string-length))

(check-typed type (typed type type))

(check-typed void (typed type void))
(check-typed boolean (typed type boolean))
(check-typed integer (typed type integer))
(check-typed char (typed type char))
(check-typed string (typed type string))

(check-typed
  (procedure () boolean)
  (typed type (procedure () boolean)))

(check-typed
  (procedure (integer string) boolean)
  (typed type (procedure (integer string) boolean)))

(check-typed
  (procedure integer boolean)
  (typed type (procedure integer boolean)))

(check-typed
  (procedure (integer char . string) boolean)
  (typed type (procedure (integer char . string) boolean)))

(check-typed (void) (typed void (void)))
(check-typed #f (typed boolean #f))
(check-typed 123 (typed integer 123))
(check-typed #\a (typed char #\a))
(check-typed "foo" (typed string "foo"))

(check-typed + (typed (procedure integer integer) %+))
(check-typed string-append (typed (procedure string string) %string-append))
(check-typed string-length (typed (procedure (string) integer) %string-length))

(check-typed
  (lambda () "foo")
  (typed
    (procedure () string)
    (lambda ()"foo")))

(check-typed
  (lambda ((integer i) (string s)) i)
  (typed
    (procedure (integer string) integer)
    (lambda (i s) i)))

(check-typed
  (lambda ((integer i) (string s)) s)
  (typed
    (procedure (integer string) string)
    (lambda (i s) s)))

(check-typed
  (lambda ((string s)) (string-length s))
  (typed
    (procedure (string) integer)
    (lambda (s) (%string-length s))))

(check-typed
  ((lambda ((string s)) (string-length s)) "foo")
  (typed
    integer
    ((lambda (s) (%string-length s)) "foo")))

(check-typed
  (string-append)
  (typed string (%string-append)))

(check-typed
  (string-append "a" "b" "c")
  (typed string (%string-append "a" "b" "c")))
