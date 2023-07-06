(import 
  (micascheme) 
  (term)
  (type))

; --------------------------------------------------------------

(check (matches? (native `(foo bar)) (native `(foo bar))))
(check (not (matches? (native `(foo bar)) (native `(foo gar)))))
(check (not (matches? (native `(foo bar)) `(foo bar))))

(check (matches? `foo `foo))
(check (not (matches? `foo `bar)))

(check (matches? (any-boolean) (any-boolean)))
(check (not (matches? (any-boolean) `foo)))

(check (matches? (any-number) (any-number)))
(check (not (matches? (any-number) `foo)))

(check (matches? (any-string) (any-string)))
(check (not (matches? (any-string) `foo)))

(check (matches? (any-type) (any-type)))
(check (not (matches? (any-type) `foo)))

(check (matches? (any-tuple `foo (list `t1 `t2)) (any-tuple `foo (list `t1 `t2))))
(check (not (matches? (any-tuple `foo (list `t1 `t2)) (any-tuple `bar (list `t1 `t2)))))
(check (not (matches? (any-tuple `foo (list `t1 `t2)) (any-tuple `foo (list `t1 `t3)))))
(check (not (matches? (any-tuple `foo (list `t1 `t2)) `foo)))

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

; ------------------------------------------

(check (eq? (type-selector (any-boolean)) `boolean))
(check (eq? (type-selector (any-number)) `number))
(check (eq? (type-selector (any-string)) `string))
(check (eq? (type-selector (any-type)) `type))
(check (eq? (type-selector (list `foo (any-number))) `foo))
(check (eq? (type-selector `foo) `foo))
(check (eq? (type-selector 123) #f))

; ------------------------------------------

(check (eq? (type-selector-index `(foo ,(any-string) ,(any-number)) `string) 0))
(check (eq? (type-selector-index `(foo ,(any-string) ,(any-number)) `number) 1))
(check (eq? (type-selector-index `(foo ,(any-string) ,(any-number)) `boolean) #f))
(check (eq? (type-selector-index (any-string) `string) #f))
