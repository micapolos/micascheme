(import (micascheme) (term) (type))

; --------------------------------------------------------------

(check (type-static? `foo))
(check (not (type-static? boolean!)))
(check (not (type-static? number!)))
(check (not (type-static? string!)))
(check (not (type-static? (universe 0))))
(check (not (type-static? (variable 0))))
(check (type-static? (function 2 `foo)))
(check (not (type-static? (function 2 number!))))
(check (type-static? (function 1 `foo)))
(check (not (type-static? (function 1 (variable 0)))))
(check (type-static? (function-type! (fn number!) `foo)))
(check (not (type-static? (function-type! (fn number!) number!))))
(check (not (type-static? (function-type! (fn number!) (variable 0)))))
(check (type-static? (tuple-type! (foo))))
(check (type-static? (tuple-type! (vec `foo `bar))))
(check (not (type-static? (tuple-type! (vec `foo number!)))))
(check (not (type-static? (tuple-type! (vec number! `bar)))))

; --------------------------------------------------------------

(check (obj=? (types-find-from (list string! `foo number!) (partial obj=? string!) 3) (box 3)))
(check (obj=? (types-find-from (list string! `foo number!) (partial obj=? `foo) 3) (box #f)))
(check (obj=? (types-find-from (list string! `foo number!) (partial obj=? number!) 3) (box 4)))
(check (obj=? (types-find-from (list string! `foo number!) (partial obj=? boolean!) 3) #f))

; --------------------------------------------------------------

(check 
  (obj=? 
    (types-indexed (list string! `foo number! boolean!))
    (list (indexed string! 0) (indexed number! 2) (indexed boolean! 3))))

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

(check (matches? (function 2 `foo) (function 2 `foo)))
(check (not (matches? (function 2 `foo) (function 3 `foo))))
(check (not (matches? (function 2 `foo) (function 2 `bar))))

(check (matches? (function 1 `foo) `foo))
(check (not (matches? (function 1 `foo) `bar)))

(check (matches? (function 2 (variable 0)) `foo))

(check (matches? (function 2 (function-type! (fn (variable 0)) (variable 0))) (function-type! (fn `foo) `foo)))
(check (not (matches? (function 1 (function-type! (fn (variable 0)) (variable 0))) (function-type! (fn `foo) `bar))))

(check (matches? (function 2 (function-type! (fn (variable 0)) (variable 1))) (function-type! (fn `foo) `foo)))
(check (matches? (function 2 (function-type! (fn (variable 0)) (variable 1))) (function-type! (fn `foo) `bar)))

(check (matches? (function 2 (variable 0)) (function 2 (variable 0))))
(check (not (matches? (function 2 (variable 0)) (function 2 (variable 1)))))

; ------------------------------------------

(check (eq? (type-selector boolean!) `boolean))
(check (eq? (type-selector number!) `number))
(check (eq? (type-selector string!) `string))
(check (eq? (type-selector (universe 3)) `universe))
(check (eq? (type-selector (function-type! (fn number!) string!)) `fn))
(check (eq? (type-selector (tuple-type! (foo number!))) `foo))
(check (eq? (type-selector `foo) `foo))
(check (eq? (type-selector 123) #f))

; ------------------------------------------

(check (eq? (type-selector-index `(foo ,string! ,number!) `string) 0))
(check (eq? (type-selector-index `(foo ,string! ,number!) `number) 1))
(check (eq? (type-selector-index `(foo ,string! ,number!) `boolean) #f))
(check (eq? (type-selector-index string! `string) #f))

; ------------------------------------------

(check (eq? (choice-type-index-of (choice-type! boolean! number!) boolean!) 0))
(check (eq? (choice-type-index-of (choice-type! boolean! number!) number!) 1))
(check (eq? (choice-type-index-of (choice-type! boolean! number!) string!) #f))
