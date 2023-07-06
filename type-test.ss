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

(check (matches? (boolean-type) (boolean-type)))
(check (not (matches? (boolean-type) `foo)))

(check (matches? (number-type) (number-type)))
(check (not (matches? (number-type) `foo)))

(check (matches? (string-type) (string-type)))
(check (not (matches? (string-type) `foo)))

(check (matches? (type-type) (type-type)))
(check (not (matches? (type-type) `foo)))

(check (matches? (tuple-type `foo (list `t1 `t2)) (tuple-type `foo (list `t1 `t2))))
(check (not (matches? (tuple-type `foo (list `t1 `t2)) (tuple-type `bar (list `t1 `t2)))))
(check (not (matches? (tuple-type `foo (list `t1 `t2)) (tuple-type `foo (list `t1 `t3)))))
(check (not (matches? (tuple-type `foo (list `t1 `t2)) `foo)))

(check (matches? (abstraction 2 `foo) (abstraction 2 `foo)))
(check (not (matches? (abstraction 2 `foo) (abstraction 3 `foo))))
(check (not (matches? (abstraction 2 `foo) (abstraction 2 `bar))))

(check (matches? (abstraction 1 `foo) `foo))
(check (not (matches? (abstraction 1 `foo) `bar)))

(check (matches? (abstraction 2 (variable 0)) `foo))

(check (matches? (abstraction 2 (arrow! (f (variable 0)) (variable 0))) (arrow! (f `foo) `foo)))
(check (not (matches? (abstraction 2 (arrow! (f (variable 0)) (variable 0))) (arrow! (g `foo) `foo))))
(check (not (matches? (abstraction 1 (arrow! (f (variable 0)) (variable 0))) (arrow! (f `foo) `bar))))

(check (matches? (abstraction 2 (arrow! (f (variable 0)) (variable 1))) (arrow! (f `foo) `foo)))
(check (matches? (abstraction 2 (arrow! (f (variable 0)) (variable 1))) (arrow! (f `foo) `bar)))

(check (matches? (abstraction 2 (variable 0)) (abstraction 2 (variable 0))))
(check (not (matches? (abstraction 2 (variable 0)) (abstraction 2 (variable 1)))))

; ------------------------------------------

(check (eq? (type-selector (boolean-type)) `boolean))
(check (eq? (type-selector (number-type)) `number))
(check (eq? (type-selector (string-type)) `string))
(check (eq? (type-selector (type-type)) `type))
(check (eq? (type-selector (list `foo (number-type))) `foo))
(check (eq? (type-selector `foo) `foo))
(check (eq? (type-selector 123) #f))

; ------------------------------------------

(check (eq? (type-selector-index `(foo ,(string-type) ,(number-type)) `string) 0))
(check (eq? (type-selector-index `(foo ,(string-type) ,(number-type)) `number) 1))
(check (eq? (type-selector-index `(foo ,(string-type) ,(number-type)) `boolean) #f))
(check (eq? (type-selector-index (string-type) `string) #f))
