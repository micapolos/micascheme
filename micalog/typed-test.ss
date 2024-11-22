(import
  (micascheme)
  (micalog typed)
  (prefix (micalog keywords) %))

(define-case-syntax (check-typed (name in) out)
  #`(check
    (equal?
      (syntax->datum (#,(identifier-append #'name #'name #'->typed) #'in))
      'out)))

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
