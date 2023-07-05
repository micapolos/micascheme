(import 
  (micascheme) 
  (term)
  (type))

; --------------------------------------------------------------

(check (matches? `foo `foo))
(check (not (matches? `foo `bar)))

(check (matches? (abstraction 2 `foo) (abstraction 2`foo)))
(check (not (matches? (abstraction 2 `foo) (abstraction 3 `foo))))
(check (not (matches? (abstraction 2 `foo) (abstraction 2 `bar))))

(check (matches? (abstraction 1 `foo) `foo))
(check (not (matches? (abstraction 1 `foo) `bar)))

(check (matches? (abstraction 2 (variable 0)) `foo))
(check (matches? (abstraction 2 (arrow (variable 0) (variable 0))) (arrow `foo `foo)))
(check (not (matches? (abstraction 1 (arrow (variable 0) (variable 0))) (arrow `foo `bar))))

(check (matches? (abstraction 2 (arrow (variable 0) (variable 1))) (arrow `foo `foo)))
(check (matches? (abstraction 2 (arrow (variable 0) (variable 1))) (arrow `foo `bar)))

(check (matches? (abstraction 2 (variable 0)) (abstraction 2 (variable 0))))
(check (not (matches? (abstraction 2 (variable 0)) (abstraction 2 (variable 1)))))
