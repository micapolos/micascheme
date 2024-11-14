(import (micascheme) (micac expand) (micac syntax) (micac env) (micac scope))

(define-aux-keywords zero one two)

(define $env
  (fluent (empty-env)
    (env+ #'bar #'bar)
    (env+ #'ten #'ten)
    (env+ #'a #'a)
    (env+ #'b #'b)
    (env+ #'c #'c)
    (env+ #'x #'x)
    (env+ #'gar #'gar)
    (env+ #'zero (lambda _ #'0))
    (env+ #'one (lambda _ #'1))
    (env+ #'two (lambda _ #'2))
    (env+ #'foo
      (syntax-rules ()
        ((_ xs ...) (bar xs ...))))))

(define-rule-syntax (check-expand-expr in out)
  (check
    (equal?
      (syntax->datum (expand-expr $env #'in))
      'out)))

(define-rule-syntax (check-expand-instr in out)
  (check
    (equal?
      (syntax->datum (expand-instr $env #'in))
      'out)))

(define-rule-syntax (check-expand-instrs (in-body ...) (out-body ...))
  (check
    (equal?
      (syntax->datum #`(#,@(expand-instrs $env #'(in-body ...))))
      '(out-body ...))))

(check-expand-expr zero 0)
(check-expand-expr one 1)
(check-expand-expr (+ zero one two) 3)
(check-expand-expr (foo zero one two) (bar 0 1 2))

(check-expand-expr (cast int 0) (cast int 0))
(check-expand-expr (cast int one) (cast int 1))
(check-expand-expr (cast zero one) (cast zero 1))

(check-expand-expr (+) 0)
(check-expand-expr (+ one) 1)
(check-expand-expr (+ one two) 3)
(check-expand-expr (+ zero one two) 3)
(check-expand-expr (+ one ten) (+ 1 ten))

(check-expand-expr (- one) -1)
(check-expand-expr (- one two) -1)
(check-expand-expr (- zero one two) -3)
(check-expand-expr (- one ten) (- 1 ten))

(check-expand-expr (*) 1)
(check-expand-expr (* two) 2)
(check-expand-expr (* one two) 2)
(check-expand-expr (* zero one two) 0)
(check-expand-expr (* one ten) (* 1 ten))

(check-expand-expr (if a b c) (if a b c))
(check-expand-expr (if zero one two) (if 0 1 2))
(check-expand-expr (if #t one two) 1)
(check-expand-expr (if #f one two) 2)
(check-expand-expr (if (and #t #f) one two) 2)

(check-expand-instr (bar 0) (bar 0))
(check-expand-instr (foo 0) (bar 0))
(check-expand-instr (foo zero) (bar 0))
(check-expand-instr (var int x) (var int x))
(check-expand-instr (var int x zero) (var int x 0))
(check-expand-instr (const int x zero) (const int x 0))

(check-expand-instr (var int (* x)) (var int (* x)))
(check-expand-instr (var int (* zero)) (var int (* zero)))
(check-expand-instr (var zero (* x)) (var zero (* x)))

(check-expand-instr (var int (* x 10)) (var int (* x 10)))
(check-expand-instr (var int (* x one)) (var int (* x 1)))
(check-expand-instr (var int (* (* x one) two)) (var int (* (* x 1) 2)))

(check-expand-instr (begin) (begin))

(check-expand-instr
  (begin (var int x zero) (var int y one))
  (begin (var int x 0) (var int y 1)))

(check-expand-instr
  (if (foo zero) (then (foo one)) (else (foo two)))
  (if (bar 0) (then (bar 1)) (else (bar 2))))

(check-expand-instr
  (when (foo zero) (foo one) (foo two))
  (when (bar 0) (bar 1) (bar 2)))

(check-expand-instr
  (while (foo zero) (foo one) (foo two))
  (while (bar 0) (bar 1) (bar 2)))

(check-expand-instr (set x (foo zero)) (set x (bar 0)))
(check-expand-instr (set x + (foo zero)) (set x + (bar 0)))
(check-expand-instr (set x - (foo zero)) (set x - (bar 0)))
(check-expand-instr (set x * (foo zero)) (set x * (bar 0)))
(check-expand-instr (set x / (foo zero)) (set x / (bar 0)))

(check-expand-instr (set (x a (zero) *) (foo one)) (set (x a (0) *) (bar 1)))
(check-expand-instr (set x (ref x a (zero) *)) (set x (ref x a (0) *)))

(check-expand-instr
  (begin
    (defer (foo zero) (foo one))
    (foo two))
  (begin
    (bar 2)
    (bar 0)
    (bar 1)))

(check-expand-instr
  (begin
    (break-if (foo zero) (foo one))
    (foo two))
  (begin
    (if (bar 0) (then (bar 1)) (else (bar 2)))))

(check-expand-instr
  (begin
    (macro ten 10)
    (set x ten))
  (begin
    (set x 10)))

(check-expand-instr
  (begin
    (macro (goo x) (gar x))
    (set x (goo zero)))
  (begin
    (set x (gar 0))))

; macro shadowed by variable declaration
(check-expand-instr
  (begin
    (macro ten 10)
    (set x ten)
    (var int ten)
    (set x ten))
  (begin
    (set x 10)
    (var int ten)
    (set x ten)))
