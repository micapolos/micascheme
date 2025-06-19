(import
  (rename (micascheme)
    (+ %+)
    (string-append %string-append)
    (string-length %string-length))
  (asm-2 typed))

(define-syntax (typed->datum $syntax $lookup)
  (syntax-case $syntax ()
    ((_ expr)
      #`'#,(syntax->typed $lookup #'expr))))

(define-syntax (check-typed $syntax $lookup)
  (syntax-case $syntax ()
    ((_ in out)
      #`(check
        (equal?
          '#,(syntax->typed $lookup #'in)
          'out)))))

(define-typed + (typed (function integer integer) %+))
(define-typed string-append (typed (function string string) %string-append))
(define-typed string-length (typed (function (string) integer) %string-length))

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
    (lambda ()"foo")))

(check-typed
  (lambda ((integer i) (string s)) i)
  (typed
    (function (integer string) integer)
    (lambda (i s) i)))

(check-typed
  (lambda ((integer i) (string s)) s)
  (typed
    (function (integer string) string)
    (lambda (i s) s)))

(check-typed
  (lambda ((string s)) (string-length s))
  (typed
    (function (string) integer)
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

(check
  (equal?
    (typed->datum (macro string-append))
    `(typed (macro ,%string-append) #f)))
