(import 
  (micascheme) 
  (variable)
  (term)
  (type) 
  (compiler))

(define-syntax-rule (check-parse lhs rhs)
  (check (obj=? (parse! lhs) rhs)))

; === typed! ===

(check (obj=? (typed! #t) (typed #t boolean!)))
(check (obj=? (typed! 128) (typed 128 number!)))
(check (obj=? (typed! "foo") (typed "foo" string!)))

; === typed-tuple! ===

(check 
  (obj=? 
    (typed-tuple! (foo))
    (typed #f (tuple-type! foo))))

(check 
  (obj=? 
    (typed-tuple! (foo (typed! "bar")))
    (typed "bar" (tuple-type! foo string!))))

(check 
  (obj=? 
    (typed-tuple! (foo (typed! "bar") (typed! 128)))
    (typed (cons "bar" 128) (tuple-type! foo string! number!))))

(check 
  (obj=? 
    (typed-tuple! (foo (typed! "bar") (typed! 128) (typed! #t)))
    (typed (vector "bar" 128 #t) (tuple-type! foo string! number! boolean!))))

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

; === literals ===

(check-parse #t (typed #t boolean!))

(check-parse 123 (typed 123 number!))

(check-parse "foo" (typed "foo" string!))

(check-parse foo (typed #f `foo))

; === native ===

(check-parse
  (native string-length (function (length string) number))
  (typed
    (native `string-length)
    (function-type! (length string!) number!)))

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
  (typed
    (function 2 
      (cons (variable 0) (variable 1)))
    (function-type! 
      (id number! string!) 
      (tuple-type! done string! number!))))

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
    (native string-length (function (length string) number))
    (length "foo"))
  (typed
    (application!
      (function 1 (application! (variable 0) "foo"))
      (native `string-length))
    number!))

; === typed-wrap ===

(check
  (obj=?
    (typed-wrap (typed "foo" string!) (choice-type! boolean! number! string!))
    (typed (cons 2 "foo") (choice-type! boolean! number! string!))))

(check
  (obj=?
    (typed-wrap (typed "foo" string!) (choice-type! boolean! number! string!))
    (typed (cons 2 "foo") (choice-type! boolean! number! string!))))

; === evaluate ===

(check (obj=? (evaluate! foo) (typed #f `foo)))

(check
  (obj=?
    (evaluate!
      (use
        (native string-length (function (length string) number))
        (native number->string (function (string number) string))
        (native string-append (function (append string string) string))
        (append (string (length "foo")) " apples")))
    (typed "3 apples" string!)))
