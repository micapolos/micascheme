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

(check (matches? boolean! boolean!))
(check (not (matches? boolean! `foo)))

(check (matches? number! number!))
(check (not (matches? number! `foo)))

(check (matches? string! string!))
(check (not (matches? string! `foo)))

(check (matches? (universe 0) (universe 0)))
(check (not (matches? (universe 0) (universe 1))))
(check (not (matches? (universe 0) `foo)))

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

(check (eq? (type-selector boolean!) `boolean))
(check (eq? (type-selector number!) `number))
(check (eq? (type-selector string!) `string))
(check (eq? (type-selector (universe 3)) `universe))
(check (eq? (type-selector (arrow! (foo number!) string!)) `function))
(check (eq? (type-selector (tuple-type! (foo number!))) `foo))
(check (eq? (type-selector `foo) `foo))
(check (eq? (type-selector 123) #f))

; ------------------------------------------

(check (eq? (type-selector-index `(foo ,string! ,number!) `string) 0))
(check (eq? (type-selector-index `(foo ,string! ,number!) `number) 1))
(check (eq? (type-selector-index `(foo ,string! ,number!) `boolean) #f))
(check (eq? (type-selector-index string! `string) #f))
