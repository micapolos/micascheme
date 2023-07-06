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
        (arrow (length string) number)))
    (typed
      (native `string-length)
      (arrow! (length string!) number!))))

; === types ===

(check 
  (obj=? 
    (parse! boolean)
    (typed boolean! type!)))

(check 
  (obj=? 
    (parse! number)
    (typed number! type!)))

(check 
  (obj=? 
    (parse! string)
    (typed string! type!)))

(check
  (obj=?
    (parse! (arrow (foo number) string))
    (typed (arrow! (foo number!) string!) type!)))

; === tuple-typeure make ===

(check
  (obj=?
    (parse! (foo 10 "bar"))
    (typed
      (tuple! 10 "bar")
      (tuple-type `foo (list number! string!)))))

; === use / get ===

(check 
  (obj=?
    (parse! (use ("foo") (get string)))
    (typed
      (application! (abstraction 1 (variable 0)) "foo")
      string!)))

; === use / application ===

(check
  (obj=?
    (parse! (use ((native string-length (arrow (length string) number))) (length "foo")))
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
        ((native string-length (arrow (length string) number))
         (native number->string (arrow (string number) string))
         (native string-append (arrow (append string string) string)))
        (append (string (length "foo")) " apples")))
    (typed "3 apples" string!)))
