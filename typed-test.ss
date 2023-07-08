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
    (parse! (foo))
    (typed (tuple!) (tuple-type! (foo)))))

(check
  (obj=?
    (parse! (foo 10))
    (typed (tuple! 10) (tuple-type! (foo number!)))))

(check
  (obj=?
    (parse! (foo 10 "bar"))
    (typed
      (tuple! 10 "bar")
      (tuple-type! (foo number! string!)))))

(check
  (obj=?
    (parse! (foo 10 "bar" 20))
    (typed
      (tuple! 10 "bar" 20)
      (tuple-type! (foo number! string! number!)))))

; === tuple-ref ===

(check
  (obj=?
    (parse! (number (point 10)))
    (typed (tuple-ref 1 (tuple! 10) 0) number!)))

(check
  (obj=?
    (parse! (number (point 10 "foo")))
    (typed (tuple-ref 2 (tuple! 10 "foo") 0) number!)))

(check
  (obj=?
    (parse! (string (point 10 "foo")))
    (typed (tuple-ref 2 (tuple! 10 "foo") 1) string!)))

(check
  (obj=?
    (parse! (number (point 10 "foo" #t)))
    (typed (tuple-ref 3 (tuple! 10 "foo" #t) 0) number!)))

(check
  (obj=?
    (parse! (string (point 10 "foo" #t)))
    (typed (tuple-ref 3 (tuple! 10 "foo" #t) 1) string!)))

(check
  (obj=?
    (parse! (boolean (point 10 "foo" #t)))
    (typed (tuple-ref 3 (tuple! 10 "foo" #t) 2) boolean!)))

; === select ===

(check
  (obj=?
    (parse! (select (not boolean) (not number) "foo"))
    (typed 
      (cons (ordinal 3 2) "foo")
      (choice-type! boolean! number! string!))))

; === switch ===

; (check
;   (obj=?
;     (parse! 
;       (switch 
;         (select (not boolean) (not number) "foo")
;         (bool boolean)
;         (num number)
;         (str string)))
;     ...))

; === function ===

(check
  (obj=?
    (parse! (function (id number string) (done string number)))
    (typed
      (function 2 
        (tuple! (variable 0) (variable 1)))
      (function-type! 
        (id number! string!) 
        (tuple-type! (done string! number!))))))

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
