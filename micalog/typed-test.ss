(import
  (micascheme)
  (micalog typed)
  (syntax scope)
  (prefix (micalog keywords) %))

(define-check-datum-> typed)
(define-check-datum-> typed-syntax)

(define $scope
  (scope-with
    (foo-1 (%wire 1))
    (foo-4 (%wire 4))
    (bar-4 (%wire 4))
    (foo-8 (%wire 8))
    (bar-8 (%wire 8))
    (reg-foo-4 (%register 4))
    (reg-bar-8 (%register 8))))

(check-typed (literal 0) (1 0))
(check-typed (literal 1) (1 1))
(check-typed (literal bin-10101100) (8 #b10101100))
(check-typed (literal hex-af) (8 #xaf))
(check-typed (literal oct-34) (6 #o34))

(check-typed (raises (literal 2)))
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

(check-typed (expr (%append bin-10 hex-af)) (%append 10 #b10 #xaf))

(check-typed (expr (%slice bin-11001010 6)) (%slice 6 #b11001010 0))
(check-typed (expr (%slice bin-11001010 8)) (%slice 8 #b11001010 0))
(check-typed (expr (%slice bin-11001010 2 4)) (%slice 4 #b11001010 2))
(check-typed (raises (expr (%slice bin-11001010 0))))
(check-typed (raises (expr (%slice bin-11001010 2 0))))
(check-typed (raises (expr (%slice bin-11001010 9))))
(check-typed (raises (expr (%slice bin-11001010 4 5))))

(check-typed (expr (%= bin-1101 hex-a)) (%= 4 #b1101 #xa))
(check-typed (expr (%!= bin-1101 hex-a)) (%!= 4 #b1101 #xa))
(check-typed (expr (%< bin-1101 hex-a)) (%< 4 #b1101 #xa))
(check-typed (expr (%<= bin-1101 hex-a)) (%<= 4 #b1101 #xa))
(check-typed (expr (%> bin-1101 hex-a)) (%> 4 #b1101 #xa))
(check-typed (expr (%>= bin-1101 hex-a)) (%>= 4 #b1101 #xa))

(check-typed (raises (expr (%= bin-1101 hex-af))))

(check-typed (expr (%not bin-1101)) (%not 4 #b1101))
(check-typed (expr (%and bin-1101 hex-a)) (%and 4 #b1101 #xa))
(check-typed (expr (%or bin-1101 hex-a)) (%or 4 #b1101 #xa))
(check-typed (expr (%xor bin-1101 hex-a)) (%xor 4 #b1101 #xa))
(check-typed (expr (%nand bin-1101 hex-a)) (%nand 4 #b1101 #xa))
(check-typed (expr (%nor bin-1101 hex-a)) (%nor 4 #b1101 #xa))
(check-typed (expr (%xnor bin-1101 hex-a)) (%xnor 4 #b1101 #xa))

(check-typed (expr (%- bin-1101)) (%- 4 #b1101))
(check-typed (expr (%+ bin-1101 hex-a)) (%+ 4 #b1101 #xa))
(check-typed (expr (%- bin-1101 hex-a)) (%- 4 #b1101 #xa))

(check-typed (expr (%if bin-1 bin-1101 hex-a)) (%if 4 #b1 #b1101 #xa))

(check-typed (scope-expr $scope foo-4) (4 foo-4))
(check-typed (scope-expr $scope (%= foo-4 bar-4)) (%= 4 foo-4 bar-4))
(check-typed (scope-expr $scope (%if foo-1 foo-8 bar-8)) (%if 8 foo-1 foo-8 bar-8))

(check-typed-syntax (scope-instr $scope (%set reg-foo-4 bar-4)) (%set 4 reg-foo-4 bar-4))
(check-typed-syntax (raises (scope-instr $scope (%set foo-4 bar-4))))
(check-typed-syntax (raises (scope-instr $scope (%set foo-4 bar-8))))

(check-typed-syntax (scope-instr $scope (%wire foo-4 bar-4)) (%wire 4 foo-4 bar-4))

(check-typed-syntax
  (scope-instrs $scope ((%wire goo-4 bin-1010) (%set reg-foo-4 goo-4)))
  ((%wire 4 goo-4 #b1010) (%set 4 reg-foo-4 goo-4)))

(check-typed-syntax
  (raises (scope-instrs $scope ((%set reg-4 foo-4)))))

(check-typed-syntax
  (scope-instrs $scope ((%register reg-4 4) (%set reg-4 foo-4)))
  ((%set 4 reg-4 foo-4)))

(check-typed-syntax
  (scope-instrs $scope ((%register reg-4 4) (%wire goo-4 foo-4) (%set reg-4 goo-4)))
  ((%wire 4 goo-4 foo-4) (%set 4 reg-4 goo-4)))

