(import 
  (micascheme) 
  (term)
  (type)
  (typed)
  (compiler))

(define-syntax-rule (check-parse lhs rhs)
  (check (obj=? (parse! lhs) rhs)))

; === literals ===

(check-parse #t (typed! #t))

(check-parse 123 (typed! 123))

(check-parse "foo" (typed! "foo"))

(check-parse foo (typed #f `foo))

; === native ===

(lets 
  ($typed (parse! (native string-length (function (length string) number))))
  ($term (typed-value $typed))
  ($type (typed-type $typed))
  (begin 
    (check (obj=? $type (function-type! (length string!) number!)))
    (check (native? $term))
    (check (equal? (syntax->datum (native-value $term)) `string-length))))

; === if ===

(check-parse
  (if #t "foo" "bar")
  (typed (conditional #t "foo" "bar") string!))

; === types ===

(check-parse (type boolean) (typed boolean! type!))
(check-parse (type number) (typed number! type!))
(check-parse (type string) (typed string! type!))
(check-parse (type type) (typed type! type!))

(check-parse
  (type (function (foo number) string))
  (typed (function-type! (foo number!) string!) type!))

; === tuple ===

(check-parse 
  (foo) 
  (typed #f (tuple-type! foo)))

(check-parse 
  (foo 10) 
  (typed 10 (tuple-type! foo number!)))

(check-parse
  (foo 10 "bar")
  (typed
    (cons 10 "bar")
    (tuple-type! foo number! string!)))

(check-parse
  (foo 10 "bar" 20)
  (typed
    (vector 10 "bar" 20)
    (tuple-type! foo number! string! number!)))

; === tuple-ref ===

(check-parse
  (number (point 10))
  (typed 10 number!))

(check-parse
  (number (point 10 "foo"))
  (typed (pair-first (cons 10 "foo")) number!))

(check-parse
  (string (point 10 "foo"))
  (typed (pair-second (cons 10 "foo")) string!))

(check-parse
  (number (point 10 "foo" #t))
  (typed (vector-get (vector 10 "foo" #t) 0) number!))

(check-parse
  (string (point 10 "foo" #t))
  (typed (vector-get (vector 10 "foo" #t) 1) string!))

(check-parse
  (boolean (point 10 "foo" #t))
  (typed (vector-get (vector 10 "foo" #t) 2) boolean!))

; === select ===

(check-parse
  (select #t (not number) (not string))
  (typed
    (cons 0 #t)
    (choice-type! boolean! number! string!)))

(check-parse
  (select (not boolean) 128 (not string))
  (typed
    (cons 1 128)
    (choice-type! boolean! number! string!)))

(check-parse
  (select (not boolean) (not number) "foo")
  (typed
    (cons 2 "foo")
    (choice-type! boolean! number! string!)))

; === switch ===

(check-parse
  (switch 
      (select (not boolean) (not number) "foo") 
      "boolean" "number" string)
  (typed
    (use! (cons 2 "foo")
      (use! (pair-second v0)
        (branch! (pair-first v1) "boolean" "number" v0)))
    string!))

; === function ===

(check-parse
  (function (id number string) (done string number))
  (typed-function!
    (id number! string!)
    (typed-tuple!
      (done
        (typed (variable 0) string!)
        (typed (variable 1) number!)))))

; === apply ===

(check-parse
  (apply (function (id number string) (done string number)) 128 "foo")
  (typed-application!
    (typed-function!
      (id number! string!)
      (typed-tuple!
        (done
          (typed (variable 0) string!)
          (typed (variable 1) number!))))
    (typed! 128)
    (typed! "foo")))

; === recursive function ===

(check-parse
  (recursive string (function (id number string) string))
  (typed
    (recursive 
      (function 2 (variable 0)))
    (function-type! (id number! string!) string!)))

; === use / get ===

(check-parse
  (use "foo" string)
  (typed
    (application! (function 1 (variable 0)) "foo")
    string!))

; === use / application ===

(check-parse
  (use 
    (function (length string) 128)
    (length "foo"))
  (typed
    (application!
      (function 1 (application! (variable 0) "foo"))
      (function 1 128))
    number!))
