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
      (arrow! (length string!) number!))))

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
    (typed (arrow! (foo number!) string!) type!)))

; === tuple! ===

(check
  (obj=?
    (parse! (foo 10 "bar"))
    (typed
      (tuple! 10 "bar")
      (tuple-type `foo (list number! string!)))))

; === function ===

(check
  (obj=?
    (parse! (function (id number string) (done string number)))
    (typed
      (abstraction 2 (tuple! (variable 0) (variable 1)))
      (arrow! (id number! string!) (tuple-type! (done string! number!))))))

; === use / get ===

(check
  (obj=?
    (parse! (use ("foo") string))
    (typed
      (application! (abstraction 1 (variable 0)) "foo")
      string!)))

; === use / application ===

(check
  (obj=?
    (parse! 
      (use ((native string-length (function (length string) number))) 
        (length "foo")))
    (typed
      (application!
        (abstraction 1 (application! (variable 0) "foo"))
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
