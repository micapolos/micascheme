(import 
  (micascheme) 
  (type))

(assert (matches? `foo `foo))
(assert (not (matches? `foo `bar)))

(assert (matches? (abstraction `foo) (abstraction `foo)))
(assert (not (matches? (abstraction `foo) (abstraction `bar))))

(assert (matches? (abstraction `foo) `foo))
(assert (not (matches? (abstraction `foo) `bar)))

(assert (matches? (abstraction (variable 0)) `foo))
(assert (matches? (abstraction (arrow (variable 0) (variable 0))) (arrow `foo `foo)))
(assert (not (matches? (abstraction (arrow (variable 0) (variable 0))) (arrow `foo `bar))))

(assert (matches? (abstraction (abstraction (arrow (variable 0) (variable 1)))) (arrow `foo `foo)))
(assert (matches? (abstraction (abstraction (arrow (variable 0) (variable 1)))) (arrow `foo `bar)))

(assert (matches? (abstraction (abstraction (variable 0))) (abstraction (abstraction (variable 0)))))
(assert (not (matches? (abstraction (abstraction (variable 0))) (abstraction (abstraction (variable 1))))))
