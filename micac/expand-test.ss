(import (micascheme) (micac expand) (micac syntax) (micac scope))

(parameterize ((scope-pretty-gen? #t))

(define-aux-keywords zero one two)

(define $scope
  (fluent (empty-scope)
    (scope+ #'bar #'bar)
    (scope+ #'ten #'ten)
    (scope+ #'a #'a)
    (scope+ #'b #'b)
    (scope+ #'c #'c)
    (scope+ #'x #'x)
    (scope+ #'gar #'gar)
    (scope+ #'zero (lambda _ #'0))
    (scope+ #'one (lambda _ #'1))
    (scope+ #'two (lambda _ #'2))
    (scope+ #'foo
      (syntax-rules ()
        ((_ xs ...) (bar xs ...))))))

(define-rule-syntax (check-expr in out)
  (check
    (equal?
      (syntax->datum (expand-expr $scope #'in))
      'out)))

(define-rule-syntax (check-instr in out)
  (check
    (equal?
      (syntax->datum (expand-instr $scope #'in))
      'out)))

(define-rule-syntax (check-instrs (in-body ...) (out-body ...))
  (check
    (equal?
      (syntax->datum #`(#,@(expand-instrs $scope #'(in-body ...))))
      '(out-body ...))))

(check-expr zero 0)
(check-expr one 1)
(check-expr (+ zero one two) 3)
(check-expr (foo zero one two) (bar 0 1 2))

(check-expr (cast int 0) (cast int 0))
(check-expr (cast int one) (cast int 1))
(check-expr (cast zero one) (cast zero 1))

(check-expr (+) 0)
(check-expr (+ one) 1)
(check-expr (+ one two) 3)
(check-expr (+ zero one two) 3)
(check-expr (+ one ten) (+ 1 ten))

(check-expr (- one) -1)
(check-expr (- one two) -1)
(check-expr (- zero one two) -3)
(check-expr (- one ten) (- 1 ten))

(check-expr (*) 1)
(check-expr (* two) 2)
(check-expr (* one two) 2)
(check-expr (* zero one two) 0)
(check-expr (* one ten) (* 1 ten))

(check-expr (if a b c) (if a b c))
(check-expr (if zero one two) (if 0 1 2))
(check-expr (if #t one two) 1)
(check-expr (if #f one two) 2)
(check-expr (if (and #t #f) one two) 2)

(check-instr (bar 0) (bar 0))
(check-instr (foo 0) (bar 0))
(check-instr (foo zero) (bar 0))
(check-instr (var int x) (var int x))
(check-instr (var int x zero) (var int x 0))
(check-instr (const int x zero) (const int x 0))

(check-instr (var int (* x)) (var int (* x)))
(check-instr (var int (* zero)) (var int (* zero)))
(check-instr (var zero (* x)) (var zero (* x)))

(check-instr (var int (* x 10)) (var int (* x 10)))
(check-instr (var int (* x one)) (var int (* x 1)))
(check-instr (var int (* (* x one) two)) (var int (* (* x 1) 2)))

(check-instr (begin) (begin))

(check-instr
  (begin (var int x zero) (var int y one))
  (begin (var int x 0) (var int y 1)))

(check-instr
  (if (foo zero) (then (foo one)) (else (foo two)))
  (if (bar 0) (then (bar 1)) (else (bar 2))))

(check-instr
  (when (foo zero) (foo one) (foo two))
  (when (bar 0) (bar 1) (bar 2)))

(check-instr
  (while (foo zero) (foo one) (foo two))
  (while (bar 0) (bar 1) (bar 2)))

(check-instr (set x (foo zero)) (set x (bar 0)))
(check-instr (set x + (foo zero)) (set x + (bar 0)))
(check-instr (set x - (foo zero)) (set x - (bar 0)))
(check-instr (set x * (foo zero)) (set x * (bar 0)))
(check-instr (set x / (foo zero)) (set x / (bar 0)))

(check-instr (set (x a (zero) *) (foo one)) (set (x a (0) *) (bar 1)))
(check-instr (set x (ref x a (zero) *)) (set x (ref x a (0) *)))

(check-instr
  (begin
    (defer (foo zero) (foo one))
    (foo two))
  (begin
    (bar 2)
    (bar 0)
    (bar 1)))

(check-instr
  (begin
    (break-if (foo zero) (foo one))
    (foo two))
  (begin
    (if (bar 0) (then (bar 1)) (else (bar 2)))))

(check-instr
  (begin
    (break-if (foo zero) (foo one))
    (foo two))
  (begin
    (if (bar 0) (then (bar 1)) (else (bar 2)))))

(check-instr
  (begin
    (macro ten 10)
    (set x ten))
  (begin
    (set x 10)))

(check-instr
  (begin
    (macro (goo x) (gar x))
    (set x (goo zero)))
  (begin
    (set x (gar 0))))

; macro shadowed by variable declaration
(check-instr
  (begin
    (macro ten 10)
    (set x ten)
    (var int ten)
    (set x ten))
  (begin
    (set x 10)
    (var int ten)
    (set x ten)))

(check-instr
  (begin
    (defer (foo zero))
    (foo one))
  (begin
    (bar 1)
    (bar 0)))

(check-instr
  (begin
    (break-if (foo zero) (foo one) (foo one))
    (foo two)
    (foo two))
  (begin
    (if (bar 0)
      (then (bar 1) (bar 1))
      (else (bar 2) (bar 2)))))

(check-instr
  (begin (defer (a)) (b))
  (begin (b) (a)))

(check-instr
  (begin (break-if x (a)) (defer (b)) (c))
  (begin (if x (then (a)) (else (c) (b)))))

)
