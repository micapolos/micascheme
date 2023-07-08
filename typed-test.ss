(import 
  (micascheme) 
  (term)
  (type) 
  (typed))

; === literals ===

(check 
  (obj=? 
    (parse! #t)
    (typed #t boolean!)))

(check 
  (obj=? 
    (parse! 123)
    (typed 123 number!)))

(check 
  (obj=? 
    (parse! "foo")
    (typed "foo" string!)))

(check 
  (obj=? 
    (parse! foo)
    (typed #f `foo)))

; === native ===

(check
  (obj=?
    (parse!
      (native string-length
        (function (length string) number)))
    (typed
      (native `string-length)
      (function-type! (length string!) number!))))

; === types ===

(check
  (obj=? 
    (parse! (type boolean))
    (typed boolean! type!)))

(check 
  (obj=? 
    (parse! (type number))
    (typed number! type!)))

(check 
  (obj=? 
    (parse! (type string))
    (typed string! type!)))

(check 
  (obj=? 
    (parse! (type type))
    (typed type! type!)))

(check
  (obj=?
    (parse! (type (function (foo number) string)))
    (typed (function-type! (foo number!) string!) type!)))

; === tuple ===

(check
  (obj=?
    (parse! (foo 10 "bar"))
    (typed
      (tuple! (foo (typed 10 number!) (typed "bar" string!)))
      (tuple-type! (foo number! string!)))))

; === tuple-get ===

(check
  (obj=?
    (parse! (number (point 10 "foo")))
    (typed
      (tuple-ref 
        (typed 
          (tuple! (point (typed 10 number!) (typed "foo" string!)))
          (tuple-type! (point number! string!))) 
        0)
      number!)))

(check
  (obj=?
    (parse! (string (point 10 "foo")))
    (typed
      (tuple-ref 
        (typed 
          (tuple! (point (typed 10 number!) (typed "foo" string!)))
          (tuple-type! (point number! string!)))
        1)
      string!)))

(check
  (obj=?
    (parse! (x (point (x 10) (y 20))))
    (typed
      (tuple-ref 
        (typed-tuple! 
          (point
            (typed-tuple! (x (typed! 10)))
            (typed-tuple! (y (typed! 20)))))
        0)
      (tuple-type! (x number!)))))

; === select ===

(check
  (obj=?
    (parse! (select (not boolean) (not number) "foo"))
    (typed 
      (select 3 2 "foo")
      (choice-type! boolean! number! string!))))

; === function ===

(check
  (obj=?
    (parse! (function (id number string) (done string number)))
    (typed-function! (id number! string!)
      (typed-tuple! 
        (done
          (typed (variable 0) string!) 
          (typed (variable 1) number!))))))

; === use / get ===

(check
  (obj=?
    (parse! (use ("foo") string))
    (typed
      (application! (function 1 (variable 0)) "foo")
      string!)))

; === use / application ===

(check
  (obj=?
    (parse! 
      (use 
        ((native string-length (function (length string) number))) 
        (length "foo")))
    (typed
      (application!
        (function 1 (application! (variable 0) "foo"))
        (native `string-length))
      number!)))

; === evaluate ===

(check (obj=? (evaluate! foo) (typed #f `foo)))

(check
  (obj=?
    (evaluate!
      (use
        ((native string-length (function (length string) number))
         (native number->string (function (string number) string))
         (native string-append (function (append string string) string)))
        (append (string (length "foo")) " apples")))
    (typed "3 apples" string!)))
