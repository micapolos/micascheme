(import 
  (micascheme) 
  (term)
  (type)
  (typed)
  (compiler))

(define-syntax-rule (check-compile lhs rhs)
  (check (obj=? (compile! lhs) rhs)))

; === literals ===

(check-compile #t (typed! #t))

(check-compile 123 (typed! 123))

(check-compile "foo" (typed! "foo"))

(check-compile foo (typed #f `foo))

; === native ===

(lets
  ($typed (compile! (native string-length (length (function (string) number)))))
  ($term (typed-value $typed))
  ($type (typed-type $typed))
  (begin
    (check (native? $term))
    (check (symbol=? (syntax->datum (native-value $term)) `string-length))
    (check (obj=? $type (tuple-type! length (function-type! (string!) number!))))))

; === if ===

(check-compile
  (if #t "foo" "bar")
  (typed (conditional #t "foo" "bar") string!))

; === types ===

(check-compile (type boolean) (typed boolean! type!))
(check-compile (type number) (typed number! type!))
(check-compile (type string) (typed string! type!))
(check-compile (type type) (typed type! type!))

(check-compile
  (type (function (number) string))
  (typed (function-type! (number!) string!) type!))

; === tuple ===

(check-compile
  (tuple foo)
  (typed #f (tuple-type! foo)))

(check-compile
  (tuple foo 10)
  (typed 10 (tuple-type! foo number!)))

(check-compile
  (tuple foo 10 "bar")
  (typed
    (cons 10 "bar")
    (tuple-type! foo number! string!)))

(check-compile
  (tuple foo 10 "bar" 20)
  (typed
    (vector 10 "bar" 20)
    (tuple-type! foo number! string! number!)))

; === tuple get ===

(check-compile
  (get (tuple foo 10) 0)
  (typed-tuple-ref (typed-tuple! (foo (typed! 10))) 0))

(check-compile
  (get (tuple foo 10 "foo") 0)
  (typed-tuple-ref (typed-tuple! (foo (typed! 10) (typed! "foo"))) 0))

(check-compile
  (get (tuple foo 10 "foo") 1)
  (typed-tuple-ref (typed-tuple! (foo (typed! 10) (typed! "foo"))) 1))

; === implicit tuple ===

(check-compile
  (foo)
  (typed #f (tuple-type! foo)))

(check-compile
  (foo 10)
  (typed 10 (tuple-type! foo number!)))

(check-compile
  (foo 10 "bar")
  (typed
    (cons 10 "bar")
    (tuple-type! foo number! string!)))

(check-compile
  (foo 10 "bar" 20)
  (typed
    (vector 10 "bar" 20)
    (tuple-type! foo number! string! number!)))

; === tuple-ref ===

(check-compile
  (number (point 10))
  (typed 10 number!))

(check-compile
  (number (point 10 "foo"))
  (typed (pair-first (cons 10 "foo")) number!))

(check-compile
  (string (point 10 "foo"))
  (typed (pair-second (cons 10 "foo")) string!))

(check-compile
  (number (point 10 "foo" #t))
  (typed (vector-get (vector 10 "foo" #t) 0) number!))

(check-compile
  (string (point 10 "foo" #t))
  (typed (vector-get (vector 10 "foo" #t) 1) string!))

(check-compile
  (boolean (point 10 "foo" #t))
  (typed (vector-get (vector 10 "foo" #t) 2) boolean!))

; === tuple first ===

; (check-compile
;   (first number (point 10 "foo" 20))
;   (typed (pair-first (vector 10 "foo" 20)) number!))

; (check-compile
;   (second number (point 10 "foo" 20))
;   (typed (pair-second (vector 10 "foo" 20)) number!))

; (check-compile
;   (nth 2 number (point 10 "foo" 20))
;   (typed (pair-second (vector 10 "foo" 20)) number!))

; === select ===

(check-compile
  (select #t (not number) (not string))
  (typed
    (cons 0 #t)
    (choice-type! boolean! number! string!)))

(check-compile
  (select (not boolean) 128 (not string))
  (typed
    (cons 1 128)
    (choice-type! boolean! number! string!)))

(check-compile
  (select (not boolean) (not number) "foo")
  (typed
    (cons 2 "foo")
    (choice-type! boolean! number! string!)))

; === switch ===

(check-compile
  (switch
      (select (not boolean) (not number) "foo")
      "boolean" "number" string)
  (typed
    (use! (cons 2 "foo")
      (use! (pair-second v0)
        (branch! (pair-first v1) "boolean" "number" v0)))
    string!))

; === function ===

(check-compile
  (function (number string) (done string number))
  (typed-function!
    (number! string!)
    (typed-tuple!
      (done
        (typed (variable 0) string!)
        (typed (variable 1) number!)))))

; === apply ===

(check-compile
  (apply (function (number string) (done string number)) 128 "foo")
  (typed-application!
    (typed-function!
      (number! string!)
      (typed-tuple!
        (done
          (typed (variable 0) string!)
          (typed (variable 1) number!))))
    (typed! 128)
    (typed! "foo")))

; === recursive function ===

(check-compile
  (recursive string "foo")
  (typed (recursive "foo") string!))

(check-compile
  (recursive string string)
  (typed (recursive (variable 0)) string!))

; === use / get ===

(check-compile
  (use "foo" string)
  (typed
    (application! (function 1 (variable 0)) "foo")
    string!))

; === variable ===

(check-compile
  (function (string number) (variable 0))
  (typed
    (function 2 (variable 0))
    (function-type! (string! number!) number!)))

(check-compile
  (function (string number) (variable 1))
  (typed
    (function 2 (variable 1))
    (function-type! (string! number!) string!)))

; === use / application ===

(check-compile
  (use
    (length (function (string) 128))
    (length "foo"))
  (typed
    (application!
      (function 1 (application! (variable 0) "foo"))
      (function 1 128))
    number!))

; === lets ===

(check-compile
  (lets 10 20 30)
  (typed
    (application! (function 1 (application! (function 1 30) 20)) 10)
    (number-type)))
