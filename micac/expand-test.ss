(import (micascheme) (micac expand) (micac syntax))

(define-aux-keywords zero one two)

(define (lookup $id)
  (cond
    ((free-identifier=? $id #'zero) (lambda _ #'0))
    ((free-identifier=? $id #'one) (lambda _ #'1))
    ((free-identifier=? $id #'two) (lambda _ #'2))
    ((free-identifier=? $id #'foo)
      (syntax-rules ()
        ((_ xs ...) (bar xs ...))))
    (else #f)))

(define-rule-syntax (check-expand-expr in out)
  (check
    (equal?
      (syntax->datum (expand-expr lookup #'in))
      'out)))

(define-rule-syntax (check-expand-instr in out)
  (check
    (equal?
      (syntax->datum (expand-instr lookup #'in))
      'out)))

(define-rule-syntax (check-expand-instrs (in-body ...) (out-body ...))
  (check
    (equal?
      (syntax->datum #`(#,@(expand-instrs lookup #'(in-body ...))))
      '(out-body ...))))

(check-expand-expr zero 0)
(check-expand-expr one 1)
(check-expand-expr (+ zero one two) 3)
(check-expand-expr (foo zero one two) (bar 0 1 2))

(check-expand-instr (bar 0) (bar 0))
(check-expand-instr (foo 0) (bar 0))
(check-expand-instr (foo zero) (bar 0))
(check-expand-instr (var int x) (var int x))
(check-expand-instr (var int x zero) (var int x 0))
(check-expand-instr (const int x zero) (const int x 0))

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
(check-expand-instr (set+ x (foo zero)) (set+ x (bar 0)))
(check-expand-instr (set- x (foo zero)) (set- x (bar 0)))
(check-expand-instr (set* x (foo zero)) (set* x (bar 0)))
(check-expand-instr (set/ x (foo zero)) (set/ x (bar 0)))

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
