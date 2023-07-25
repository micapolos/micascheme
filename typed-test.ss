(import 
  (micascheme) 
  (term)
  (type) 
  (typed))

; === typed! ===

(check (obj=? (typed! foo) (typed #f `foo)))
(check (obj=? (typed! #t) (typed #t boolean!)))
(check (obj=? (typed! 128) (typed 128 number!)))
(check (obj=? (typed! "foo") (typed "foo" string!)))

; === types-resolve ===

(check
  (obj=? 
    (types-resolve (list number! `static string!) 
      (lambda ($typed) (and (obj=? (typed-type $typed) number!) $typed)))
    (typed (variable 0) number!)))

(check
  (obj=? 
    (types-resolve (list number! `static string!) 
      (lambda ($typed) (and (obj=? (typed-type $typed) `static) $typed)))
    (typed #f `static)))

(check
  (obj=? 
    (types-resolve (list number! `static string!) 
      (lambda ($typed) (and (obj=? (typed-type $typed) string!) $typed)))
    (typed (variable 1) string!)))

; === typed-tuple! ===

(check 
  (obj=? 
    (typed-tuple! (foo))
    (typed #f (tuple-type! (foo)))))

(check 
  (obj=? 
    (typed-tuple! (foo (typed! static)))
    (typed #f (tuple-type! (foo `static)))))

(check 
  (obj=? 
    (typed-tuple! (foo (typed! "bar")))
    (typed "bar" (tuple-type! (foo string!)))))

(check 
  (obj=? 
    (typed-tuple! (foo (typed! static1) (typed! "bar") (typed! static2)))
    (typed "bar" (tuple-type! (foo `static1 string! `static2)))))

(check 
  (obj=? 
    (typed-tuple! (foo (typed! "bar") (typed! 128)))
    (typed (cons "bar" 128) (tuple-type! (foo string! number!)))))

(check 
  (obj=? 
    (typed-tuple! (foo (typed! static1) (typed! "bar") (typed! static2) (typed! 128) (typed! static3)))
    (typed (cons "bar" 128) (tuple-type! (foo `static1 string! `static2 number! `static3)))))

(check 
  (obj=? 
    (typed-tuple! (foo (typed! "bar") (typed! 128) (typed! #t)))
    (typed (vector "bar" 128 #t) (tuple-type! (foo string! number! boolean!)))))

(check 
  (obj=? 
    (typed-tuple! (foo (typed! static1) (typed! "bar") (typed! static2) (typed! 128) (typed! static3) (typed! #t) (typed! static4)))
    (typed (vector "bar" 128 #t) (tuple-type! (foo `static1 string! `static2 number! `static3 boolean! `static4)))))

; === typed-tuple-ref ===

(check
  (obj=?
    (typed-tuple-ref (typed `tuple (tuple-type! (foo string!))) 0)
    (typed `tuple string!)))

(check
  (obj=?
    (typed-tuple-ref (typed `tuple (tuple-type! (foo string! number!))) 0)
    (typed (pair-first `tuple) string!)))

(check
  (obj=?
    (typed-tuple-ref (typed `tuple (tuple-type! (foo string! number!))) 1)
    (typed (pair-second `tuple) number!)))

(check
  (obj=?
    (typed-tuple-ref (typed `tuple (tuple-type! (foo string! number! boolean!))) 0)
    (typed (vector-get `tuple 0) string!)))

(check
  (obj=?
    (typed-tuple-ref (typed `tuple (tuple-type! (foo string! number! boolean!))) 1)
    (typed (vector-get `tuple 1) number!)))

(check
  (obj=?
    (typed-tuple-ref (typed `tuple (tuple-type! (foo string! number! boolean!))) 2)
    (typed (vector-get `tuple 2) boolean!)))

; === typed-choice! ===

(check
  (obj=?
    (typed-choice! (typed! "foo"))
    (typed
      (cons 0 "foo")
      (choice-type! string!))))

(check
  (obj=?
    (typed-choice! (not number!) (typed! "foo"))
    (typed
      (cons 1 "foo")
      (choice-type! number! string!))))

(check
  (obj=?
    (typed-choice! (not boolean!) (not number!) (typed! "foo"))
    (typed
      (cons 2 "foo")
      (choice-type! boolean! number! string!))))

; === typed-function ===

(check
  (obj=?
    (typed-function! (fn string! number!) (typed! #t))
    (typed
      (function 2 #t)
      (function-type! (fn string! number!) boolean!))))

; === typed-wrap ===

(check
  (obj=?
    (typed-wrap (typed "foo" string!) (choice-type! boolean! number! string!))
    (typed (cons 2 "foo") (choice-type! boolean! number! string!))))

(check
  (obj=?
    (typed-wrap (typed "foo" string!) (choice-type! boolean! number! string!))
    (typed (cons 2 "foo") (choice-type! boolean! number! string!))))
