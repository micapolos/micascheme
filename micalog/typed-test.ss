(import
  (micascheme)
  (micalog typed)
  (micac scope)
  (prefix (micalog keywords) %))

(define-case-syntax (check-typed (name arg ... input) expected)
  #`(check
    (equal?
      (syntax->datum (#,(identifier-append #'name #'name #'->typed) arg ... #'input))
      'expected)))

(define $scope
  (scope-with
    (foo-4 (4 foo))
    (bar-4 (4 bar))
    (foo-8 (8 foo))
    (bar-8 (8 bar))))

(check-typed (literal 0) (1 0))
(check-typed (literal 1) (1 1))
(check-typed (literal bin-10101100) (8 #b10101100))
(check-typed (literal hex-af) (8 #xaf))
(check-typed (literal oct-34) (6 #o34))

(check-typed (expr bin-101) (3 #b101))

(check-typed (expr (%= bin-1101 hex-a)) (%= 4 #b1101 #xa))
(check-typed (expr (%!= bin-1101 hex-a)) (%!= 4 #b1101 #xa))
(check-typed (expr (%< bin-1101 hex-a)) (%< 4 #b1101 #xa))
(check-typed (expr (%<= bin-1101 hex-a)) (%<= 4 #b1101 #xa))
(check-typed (expr (%> bin-1101 hex-a)) (%> 4 #b1101 #xa))
(check-typed (expr (%>= bin-1101 hex-a)) (%>= 4 #b1101 #xa))

(check-typed (expr (%not bin-1101)) (%not 4 #b1101))
(check-typed (expr (%and bin-1101 hex-a)) (%and 4 #b1101 #xa))
(check-typed (expr (%or bin-1101 hex-a)) (%or 4 #b1101 #xa))
(check-typed (expr (%xor bin-1101 hex-a)) (%xor 4 #b1101 #xa))
(check-typed (expr (%nand bin-1101 hex-a)) (%nand 4 #b1101 #xa))
(check-typed (expr (%nor bin-1101 hex-a)) (%nor 4 #b1101 #xa))
(check-typed (expr (%xnor bin-1101 hex-a)) (%xnor 4 #b1101 #xa))

(check-typed (expr (%neg bin-1101)) (%neg 4 #b1101))
(check-typed (expr (%add bin-1101 hex-a)) (%add 4 #b1101 #xa))
(check-typed (expr (%sub bin-1101 hex-a)) (%sub 4 #b1101 #xa))

(check-typed (scope-expr $scope foo-4) (4 foo))
(check-typed (scope-expr $scope (%= foo-4 bar-4)) (%= 4 foo bar))
