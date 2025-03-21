(import
  (micascheme)
  (micalog core type)
  (syntax lookup)
  (prefix (micalog keywords) %))

(define-check-datum-> typed)
(define-check-datum-> typed-syntax)
(define-check-datum-> syntax)

(define $lookup
  (fluent (empty-lookup)
    (lookup+core)
    (lookup+undefined #'foo-1 (pair #'foo-1 #'(%wire 1)))
    (lookup+undefined #'bar-1 (pair #'bar-1 #'(%wire 1)))
    (lookup+undefined #'foo-4 (pair #'foo-4 #'(%wire 4)))
    (lookup+undefined #'bar-4 (pair #'bar-4 #'(%wire 4)))
    (lookup+undefined #'foo-8 (pair #'foo-8 #'(%wire 8)))
    (lookup+undefined #'bar-8 (pair #'bar-8 #'(%wire 8)))
    (lookup+undefined #'reg-foo-4 (pair #'reg-foo-4 #'(%register 4)))
    (lookup+undefined #'reg-bar-4 (pair #'reg-bar-4 #'(%register 4)))
    (lookup+undefined #'reg-bar-8 (pair #'reg-bar-8 #'(%register 8)))
    (lookup+undefined #'zoo-1 (pair #'renamed-zoo-1 #'(%wire 1)))))

(check-syntax (type 1) 1)
(check-syntax (type 8) 8)
(check-syntax (raises (type 0)))
(check-syntax (raises (type foo)))

(check-typed (literal 0) (1 0))
(check-typed (literal 1) (1 1))
(check-typed (literal #b10011) (5 #b10011))

(check-typed (int (%int 8 12)) (8 12))

(check-typed (literal bin-10101100) (8 #b10101100))
(check-typed (literal hex-af) (8 #xaf))
(check-typed (literal oct-34) (6 #o34))

(check-typed (raises (literal bin)))
(check-typed (raises (literal bin-)))
(check-typed (raises (literal bin-12)))
(check-typed (raises (literal hex)))
(check-typed (raises (literal hex-)))
(check-typed (raises (literal hex-3g)))
(check-typed (raises (literal oct)))
(check-typed (raises (literal oct-)))
(check-typed (raises (literal oct-08)))

(check-typed (expr 0) (1 0))
(check-typed (expr bin-101) (3 #b101))
(check-typed (expr (%int 8 12)) (8 12))

(check-typed (expr (%append bin-10 hex-af)) (10 (%append (2 #b10) (8 #xaf))))

(check-typed (expr (%take bin-11001010 6)) (6 (%take 6 #b11001010 6)))
(check-typed (expr (%take bin-11001010 8)) (8 (%take 8 #b11001010 8)))
(check-typed (raises (expr (%take bin-11001010 0))))
(check-typed (raises (expr (%take bin-11001010 9))))

(check-typed (expr (%drop bin-11001010 6)) (2 (%drop 2 #b11001010 6)))
(check-typed (expr (%drop bin-11001010 2)) (6 (%drop 6 #b11001010 2)))
(check-typed (raises (expr (%drop bin-11001010 8))))

(check-typed (expr (%= bin-1101 hex-a)) (1 (%= 4 #b1101 #xa)))
(check-typed (expr (%!= bin-1101 hex-a)) (1 (%!= 4 #b1101 #xa)))
(check-typed (expr (%< bin-1101 hex-a)) (1 (%< 4 #b1101 #xa)))
(check-typed (expr (%<= bin-1101 hex-a)) (1 (%<= 4 #b1101 #xa)))
(check-typed (expr (%> bin-1101 hex-a)) (1 (%> 4 #b1101 #xa)))
(check-typed (expr (%>= bin-1101 hex-a)) (1 (%>= 4 #b1101 #xa)))

(check-typed (expr (%= bin-1101 hex-af)) (1 (%= 8 #b1101 #xaf)))

(check-typed (expr (%not bin-1101)) (4 (%not 4 #b1101)))

(check-typed (raises (expr (%and))))
(check-typed (expr (%and bin-1101)) (4 #b1101))
(check-typed (expr (%and bin-1101 hex-a)) (4 (%and 4 #b1101 #xa)))
(check-typed (expr (%and bin-1101 hex-a hex-f)) (4 (%and 4 (%and 4 #b1101 #xa) #xf)))

(check-typed (raises (expr (%or))))
(check-typed (expr (%or bin-1101)) (4 #b1101))
(check-typed (expr (%or bin-1101 hex-a)) (4 (%or 4 #b1101 #xa)))
(check-typed (expr (%or bin-1101 hex-a hex-f)) (4 (%or 4 (%or 4 #b1101 #xa) #xf)))

(check-typed (raises (expr (%xor))))
(check-typed (expr (%xor bin-1101)) (4 #b1101))
(check-typed (expr (%xor bin-1101 hex-a)) (4 (%xor 4 #b1101 #xa)))
(check-typed (expr (%xor bin-1101 hex-a hex-f)) (4 (%xor 4 (%xor 4 #b1101 #xa) #xf)))

(check-typed (raises (expr (%nand))))
(check-typed (expr (%nand bin-1101)) (4 #b1101))
(check-typed (expr (%nand bin-1101 hex-a)) (4 (%nand 4 #b1101 #xa)))
(check-typed (expr (%nand bin-1101 hex-a hex-f)) (4 (%nand 4 (%nand 4 #b1101 #xa) #xf)))

(check-typed (raises (expr (%nor))))
(check-typed (expr (%nor bin-1101)) (4 #b1101))
(check-typed (expr (%nor bin-1101 hex-a)) (4 (%nor 4 #b1101 #xa)))
(check-typed (expr (%nor bin-1101 hex-a hex-f)) (4 (%nor 4 (%nor 4 #b1101 #xa) #xf)))

(check-typed (raises (expr (%xnor))))
(check-typed (expr (%xnor bin-1101)) (4 #b1101))
(check-typed (expr (%xnor bin-1101 hex-a)) (4 (%xnor 4 #b1101 #xa)))
(check-typed (expr (%xnor bin-1101 hex-a hex-f)) (4 (%xnor 4 (%xnor 4 #b1101 #xa) #xf)))

(check-typed (raises (expr (%wrap+))))
(check-typed (expr (%wrap+ bin-1101)) (4 #b1101))
(check-typed (expr (%wrap+ bin-1101 hex-a)) (4 (%wrap+ 4 #b1101 #xa)))
(check-typed (expr (%wrap+ bin-1101 hex-a hex-f)) (4 (%wrap+ 4 (%wrap+ 4 #b1101 #xa) #xf)))

(check-typed (raises (expr (%wrap-))))
(check-typed (expr (%wrap- bin-1101)) (4 (%wrap- 4 #b1101)))
(check-typed (expr (%wrap- bin-1101 hex-a)) (4 (%wrap- 4 #b1101 #xa)))
(check-typed (expr (%wrap- bin-1101 hex-a hex-f)) (4 (%wrap- 4 (%wrap- 4 #b1101 #xa) #xf)))

(check-typed (expr (%+ bin-1101 hex-a)) (5 (%+ 5 #b1101 #xa)))
(check-typed (expr (%- bin-1101 hex-a)) (5 (%- 5 #b1101 #xa)))
(check-typed (expr (%* bin-1101 hex-a)) (8 (%* 8 #b1101 #xa)))

(check-typed (expr (%wrap+ bin-1101 1)) (4 (%wrap+ 4 #b1101 1)))

(check-typed (expr (%if bin-1 bin-1101 hex-a)) (4 (%if 4 #b1 #b1101 #xa)))

(check-typed (lookup-expr $lookup foo-4) (4 foo-4))
(check-typed (lookup-expr $lookup zoo-1) (1 renamed-zoo-1))
(check-typed (lookup-expr $lookup (%= foo-4 bar-4)) (1 (%= 4 foo-4 bar-4)))
(check-typed (lookup-expr $lookup (%if foo-1 foo-8 bar-8)) (8 (%if 8 foo-1 foo-8 bar-8)))

; raise on re-declaration
(check-typed-syntax (raises (lookup-instr $lookup (%input %input))))
(check-typed-syntax (raises (lookup-instr $lookup (%input %wire))))
(check-typed-syntax (raises (lookup-instr $lookup (%input %if))))
(check-typed-syntax (raises (lookup-instr $lookup (%input %macro))))
(check-typed-syntax (raises (lookup-instr $lookup (%input %int))))
(check-typed-syntax (raises (lookup-instr $lookup (%input bin-000))))

(check-typed-syntax (raises (lookup-instr $lookup (%input foo-1))))

(check-typed-syntax (lookup-instr $lookup (%input moo-1)) (%input 1 moo-1))
(check-typed-syntax (lookup-instr $lookup (%input 4 moo-4)) (%input 4 moo-4))
(check-typed-syntax (raises (lookup-instr $lookup (%input foo-4 foo))))

(check-typed-syntax (lookup-instr $lookup (%wire wire-4 foo-4)) (%wire 4 wire-4 foo-4))
(check-typed-syntax (lookup-instr $lookup (%output out-4 foo-4)) (%output 4 out-4 foo-4))

(check-typed-syntax (lookup-instr $lookup (%set reg-foo-4 bin-1001)) (%set 4 reg-foo-4 #b1001))
(check-typed-syntax (lookup-instr $lookup (%set reg-foo-4 bar-4)) (%set 4 reg-foo-4 bar-4))
(check-typed-syntax (raises (lookup-instr $lookup (%set foo-4 bar-4))))
(check-typed-syntax (raises (lookup-instr $lookup (%set reg-foo-4 bar-8))))

(check-typed-syntax
  (lookup-instrs $lookup ((%wire goo-4 bin-1010) (%set reg-foo-4 goo-4)))
  ((%wire 4 goo-4 #b1010) (%set 4 reg-foo-4 goo-4)))

(check-typed-syntax
  (raises (lookup-instrs $lookup ((%set reg-4 foo-4)))))

(check-typed-syntax
  (lookup-instrs $lookup ((%register reg-1) (%set reg-1 foo-1)))
  ((%register 1 reg-1) (%set 1 reg-1 foo-1)))

(check-typed-syntax
  (lookup-instrs $lookup ((%register 4 reg-4) (%set reg-4 foo-4)))
  ((%register 4 reg-4) (%set 4 reg-4 foo-4)))

(check-typed-syntax
  (lookup-instrs $lookup ((%register 4 reg-4) (%wire goo-4 foo-4) (%set reg-4 goo-4)))
  ((%register 4 reg-4) (%wire 4 goo-4 foo-4) (%set 4 reg-4 goo-4)))

(check-typed-syntax (raises (lookup-instr $lookup (%cond (foo-4)))))

(check-typed-syntax
  (lookup-instr $lookup (%cond (foo-1)))
  (%cond (foo-1)))

(check-typed-syntax
  (lookup-instr $lookup
    (%cond
      (foo-1
        (%set reg-foo-4 foo-4)
        (%set reg-bar-8 bar-8))))
  (%cond
    (foo-1
      (%set 4 reg-foo-4 foo-4)
      (%set 8 reg-bar-8 bar-8))))

(check-typed-syntax (raises (lookup-instr $lookup (%cond (foo-4) (%else)))))

(check-typed-syntax
  (lookup-instr $lookup (%cond (foo-1) (%else)))
  (%cond (foo-1) (%else)))

(check-typed-syntax
  (lookup-instr $lookup
    (%cond
      (foo-1 (%set reg-foo-4 foo-4))
      (%else (%set reg-bar-8 bar-8))))
  (%cond
    (foo-1 (%set 4 reg-foo-4 foo-4))
    (%else (%set 8 reg-bar-8 bar-8))))

(check-typed-syntax
  (lookup-instr $lookup
    (%cond
      (foo-1 (%set reg-foo-4 foo-4))
      (bar-1 (%set reg-foo-4 bar-4))
      (%else (%set reg-bar-8 bar-8))))
  (%cond
    (foo-1 (%set 4 reg-foo-4 foo-4))
    (bar-1 (%set 4 reg-foo-4 bar-4))
    (%else (%set 8 reg-bar-8 bar-8))))

(check-typed-syntax (raises (lookup-instr $lookup (%on (%posedge foo-4)))))
(check-typed-syntax (raises (lookup-instr $lookup (%on (invalid-edge foo-1)))))

(check-typed-syntax
  (lookup-instr $lookup
    (%on (%posedge foo-1)
      (%set reg-foo-4 foo-4)))
  (%on (%posedge foo-1)
    (%set 4 reg-foo-4 foo-4)))

(check-typed-syntax
  (lookup-instr $lookup (%log foo foo-4))
  (%log foo 4 foo-4))

(check-typed-syntax
  (module (%module empty))
  (%module empty))

(check-typed-syntax
  (module
    (%module bypass-8
      (%input 8 in)
      (%output out in)))
  (%module bypass-8
    (%input 8 in)
    (%output 8 out in)))

(check-typed-syntax
  (module
    (%module and-gate
      (%input 1 clock)
      (%input 8 in-1)
      (%input 8 in-2)
      (%output out
        (%append
          (%and in-1 in-2)
          clock))))
  (%module and-gate
    (%input 1 clock)
    (%input 8 in-1)
    (%input 8 in-2)
    (%output 9 out
      (%append
        (8 (%and 8 in-1 in-2))
        (1 clock)))))

; === macros ===

(check-typed-syntax
  (lookup-instrs $lookup ((%macro (set-zero name) (%set name 0))))
  ())

(check-typed-syntax
  (lookup-instrs $lookup
    (
      (%macro (local-register param)
        (%register 8 local)
        (%set local param))
      (local-register foo-8)))
  (
    (%register 8 local_0)
    (%set 8 local_0 foo-8)))

(check-typed-syntax
  (lookup-instrs $lookup
    (
      (%macro (set-zero name) (%set name 0))
      (set-zero reg-foo-4)))
  ((%set 4 reg-foo-4 0)))

(check-typed-syntax
  (lookup-instrs $lookup
    (
      (%macro (exchange a b)
        (%set a b)
        (%set b a))
      (exchange reg-foo-4 reg-bar-4)))
  (
    (%set 4 reg-foo-4 reg-bar-4)
    (%set 4 reg-bar-4 reg-foo-4)))

(check-typed-syntax
  (lookup-instrs $lookup
    (
      (%macro (double a) (%wrap+ a a))
      (%set reg-foo-4 (double foo-4))))
  ((%set 4 reg-foo-4 (%wrap+ 4 foo-4 foo-4))))

(check-typed-syntax
  (lookup-instrs $lookup
    (
      (%macro (double expr) (%wrap+ expr expr))
      (%macro (local-register param)
        (%register 8 local)
        (%set local (double param)))
      (local-register foo-8)))
  (
    (%register 8 local_0)
    (%set 8 local_0 (%wrap+ 8 foo-8 foo-8))))

(check-typed-syntax
  (lookup-instrs $lookup
    (
      (%macro (whenek cond body (... ...))
        (%cond (cond body (... ...))))
      (whenek foo-1
        (%set reg-foo-4 foo-4)
        (%set reg-bar-4 bar-4))))
  (
    (%cond
      (foo-1
        (%set 4 reg-foo-4 foo-4)
        (%set 4 reg-bar-4 bar-4)))))

; === repeat ===

; (check-typed-syntax
;   (lookup-instrs $lookup
;     ((%repeat (i 3) (%set reg-foo-4 i))))
;   (
;     (%set 4 reg-foo-4 hex-0)
;     (%set 4 reg-foo-4 hex-1)
;     (%set 4 reg-foo-4 hex-2)))

; TODO: Not working, same variable is re-declared.
; Replace $gen? with a list of parameters, which will be preserved.
; Everything else will be generated.
; Or... add %param to the lookup.
; (check-typed-syntax
;   (lookup-instrs $lookup
;     ((%repeat (i 3)
;       (%register 4 reg)
;       (%set reg i))))
;   (
;     (%set 4 reg-foo-4 0)
;     (%set 4 reg-foo-4 1)
;     (%set 4 reg-foo-4 2)))
