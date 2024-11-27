(import
  (micascheme)
  (micalog core type)
  (syntax scope)
  (prefix (micalog keywords) %))

(define-check-datum-> typed)
(define-check-datum-> typed-syntax)
(define-check-datum-> syntax)

(define $scope
  (scope-with
    (foo-1 (%wire 1 foo-1))
    (bar-1 (%wire 1 bar-1))
    (foo-4 (%wire 4 foo-4))
    (bar-4 (%wire 4 bar-4))
    (foo-8 (%wire 8 foo-8))
    (bar-8 (%wire 8 bar-8))
    (reg-foo-4 (%register 4 reg-foo-4))
    (reg-bar-4 (%register 4 reg-bar-4))
    (reg-bar-8 (%register 8 reg-bar-8))
    (zoo-1 (%wire 1 renamed-zoo-1))))

(check-syntax (type 1) 1)
(check-syntax (type 8) 8)
(check-syntax (raises (type 0)))
(check-syntax (raises (type foo)))

(check-typed (literal 0) (1 0))
(check-typed (literal 1) (1 1))
(check-typed (literal #b10011) (5 #b10011))

(check-typed (literal (%int 8 12)) (8 12))

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

(check-typed (expr bin-101) (3 #b101))

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
(check-typed (expr (%and bin-1101 hex-a)) (4 (%and 4 #b1101 #xa)))
(check-typed (expr (%or bin-1101 hex-a)) (4 (%or 4 #b1101 #xa)))
(check-typed (expr (%xor bin-1101 hex-a)) (4 (%xor 4 #b1101 #xa)))
(check-typed (expr (%nand bin-1101 hex-a)) (4 (%nand 4 #b1101 #xa)))
(check-typed (expr (%nor bin-1101 hex-a)) (4 (%nor 4 #b1101 #xa)))
(check-typed (expr (%xnor bin-1101 hex-a)) (4 (%xnor 4 #b1101 #xa)))

(check-typed (expr (%- bin-1101)) (5 (%- 5 #b1101)))

(check-typed (expr (%+ bin-1101 hex-a)) (5 (%+ 5 #b1101 #xa)))
(check-typed (expr (%- bin-1101 hex-a)) (5 (%- 5 #b1101 #xa)))
(check-typed (expr (%* bin-1101 hex-a)) (8 (%* 8 #b1101 #xa)))

(check-typed (expr (%+ bin-1101 1)) (5 (%+ 5 #b1101 1)))

(check-typed (expr (%if bin-1 bin-1101 hex-a)) (4 (%if 4 #b1 #b1101 #xa)))

(check-typed (scope-expr $scope foo-4) (4 foo-4))
(check-typed (scope-expr $scope zoo-1) (1 renamed-zoo-1))
(check-typed (scope-expr $scope (%= foo-4 bar-4)) (1 (%= 4 foo-4 bar-4)))
(check-typed (scope-expr $scope (%if foo-1 foo-8 bar-8)) (8 (%if 8 foo-1 foo-8 bar-8)))

(check-typed-syntax (raises (scope-instr $scope (%input foo-1))))

(check-typed-syntax (scope-instr $scope (%input moo-1)) (%input 1 moo-1))
(check-typed-syntax (scope-instr $scope (%input 4 moo-4)) (%input 4 moo-4))
(check-typed-syntax (raises (scope-instr $scope (%input foo-4 foo))))

(check-typed-syntax (scope-instr $scope (%wire wire-4 foo-4)) (%wire 4 wire-4 foo-4))
(check-typed-syntax (scope-instr $scope (%output out-4 foo-4)) (%output 4 out-4 foo-4))

(check-typed-syntax (scope-instr $scope (%set reg-foo-4 bin-1001)) (%set 4 reg-foo-4 #b1001))
(check-typed-syntax (scope-instr $scope (%set reg-foo-4 bar-4)) (%set 4 reg-foo-4 bar-4))
(check-typed-syntax (raises (scope-instr $scope (%set foo-4 bar-4))))
(check-typed-syntax (raises (scope-instr $scope (%set reg-foo-4 bar-8))))

(check-typed-syntax (scope-instr $scope (%set-take reg-foo-4 bar-8)) (%set 4 reg-foo-4 (%take 4 bar-8 4)))

(check-typed-syntax
  (scope-instrs $scope ((%wire goo-4 bin-1010) (%set reg-foo-4 goo-4)))
  ((%wire 4 goo-4 #b1010) (%set 4 reg-foo-4 goo-4)))

(check-typed-syntax
  (raises (scope-instrs $scope ((%set reg-4 foo-4)))))

(check-typed-syntax
  (scope-instrs $scope ((%register reg-1) (%set reg-1 foo-1)))
  ((%register 1 reg-1) (%set 1 reg-1 foo-1)))

(check-typed-syntax
  (scope-instrs $scope ((%register 4 reg-4) (%set reg-4 foo-4)))
  ((%register 4 reg-4) (%set 4 reg-4 foo-4)))

(check-typed-syntax
  (scope-instrs $scope ((%register 4 reg-4) (%wire goo-4 foo-4) (%set reg-4 goo-4)))
  ((%register 4 reg-4) (%wire 4 goo-4 foo-4) (%set 4 reg-4 goo-4)))

(check-typed-syntax (raises (scope-instr $scope (%cond (foo-4)))))

(check-typed-syntax
  (scope-instr $scope (%cond (foo-1)))
  (%cond (foo-1)))

(check-typed-syntax
  (scope-instr $scope
    (%cond
      (foo-1
        (%set reg-foo-4 foo-4)
        (%set reg-bar-8 bar-8))))
  (%cond
    (foo-1
      (%set 4 reg-foo-4 foo-4)
      (%set 8 reg-bar-8 bar-8))))

(check-typed-syntax (raises (scope-instr $scope (%cond (foo-4) (%else)))))

(check-typed-syntax
  (scope-instr $scope (%cond (foo-1) (%else)))
  (%cond (foo-1) (%else)))

(check-typed-syntax
  (scope-instr $scope
    (%cond
      (foo-1 (%set reg-foo-4 foo-4))
      (%else (%set reg-bar-8 bar-8))))
  (%cond
    (foo-1 (%set 4 reg-foo-4 foo-4))
    (%else (%set 8 reg-bar-8 bar-8))))

(check-typed-syntax
  (scope-instr $scope
    (%cond
      (foo-1 (%set reg-foo-4 foo-4))
      (bar-1 (%set reg-foo-4 bar-4))
      (%else (%set reg-bar-8 bar-8))))
  (%cond
    (foo-1 (%set 4 reg-foo-4 foo-4))
    (bar-1 (%set 4 reg-foo-4 bar-4))
    (%else (%set 8 reg-bar-8 bar-8))))

(check-typed-syntax (raises (scope-instr $scope (%on foo-4 (%posedge)))))
(check-typed-syntax (raises (scope-instr $scope (%on foo-4 (%posedge) (%negedge)))))
(check-typed-syntax (raises (scope-instr $scope (%on foo-1 (%posedge) (%posedge)))))
(check-typed-syntax (raises (scope-instr $scope (%on foo-1 (%negedge) (%negedge)))))

(check-typed-syntax
  (scope-instr $scope
    (%on (%posedge foo-1)
      (%set reg-foo-4 foo-4)))
  (%on foo-1
    (%posedge
      (%set 4 reg-foo-4 foo-4))))

(check-typed-syntax
  (scope-instr $scope (%log foo foo-4))
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
  (scope-instrs $scope ((%macro (set-zero name) (%set name 0))))
  ())

(check-typed-syntax
  (scope-instrs $scope
    (
      (%macro (local-register param)
        (%register 8 local)
        (%set local param))
      (local-register foo-8)))
  (
    (%register 8 local_0)
    (%set 8 local_0 foo-8)))

(check-typed-syntax
  (scope-instrs $scope
    (
      (%macro (set-zero name) (%set name 0))
      (set-zero reg-foo-4)))
  ((%set 4 reg-foo-4 0)))

(check-typed-syntax
  (scope-instrs $scope
    (
      (%macro (exchange a b)
        (%set a b)
        (%set b a))
      (exchange reg-foo-4 reg-bar-4)))
  (
    (%set 4 reg-foo-4 reg-bar-4)
    (%set 4 reg-bar-4 reg-foo-4)))

(check-typed-syntax
  (scope-instrs $scope
    (
      (%macro (double a) (%+ a a))
      (%set reg-foo-4 (%take (double foo-4) 4))))
  ((%set 4 reg-foo-4 (%take 4 (%+ 5 foo-4 foo-4) 4))))

(check-typed-syntax
  (scope-instrs $scope
    (
      (%macro (double expr) (%+ expr expr))
      (%macro (local-register param)
        (%register 8 local)
        (%set local (%take (double param) 8)))
      (local-register foo-8)))
  (
    (%register 8 local_0)
    (%set 8 local_0 (%take 8 (%+ 9 foo-8 foo-8) 8))))

(check-typed-syntax
  (scope-instrs $scope
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
;   (scope-instrs $scope
;     ((%repeat (i 3) (%set reg-foo-4 i))))
;   (
;     (%set 4 reg-foo-4 hex-0)
;     (%set 4 reg-foo-4 hex-1)
;     (%set 4 reg-foo-4 hex-2)))

; TODO: Not working, same variable is re-declared.
; Replace $gen? with a list of parameters, which will be preserved.
; Everything else will be generated.
; Or... add %param to the scope.
; (check-typed-syntax
;   (scope-instrs $scope
;     ((%repeat (i 3)
;       (%register 4 reg)
;       (%set reg i))))
;   (
;     (%set 4 reg-foo-4 0)
;     (%set 4 reg-foo-4 1)
;     (%set 4 reg-foo-4 2)))
