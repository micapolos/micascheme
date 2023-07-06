(import 
  (micascheme) 
  (term)
  (type) 
  (typed))

; === literals ===

(check 
  (obj=? 
    (parse! #t)
    (typed #t (any-boolean))))

(check 
  (obj=? 
    (parse! 123)
    (typed 123 (any-number))))

(check 
  (obj=? 
    (parse! "foo")
    (typed "foo" (any-string))))

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
      (arrow
        (any-tuple `length (list (any-string)))
        (any-number)))))

; === types ===

(check 
  (obj=? 
    (parse! boolean)
    (typed (any-boolean) (any-type))))

(check 
  (obj=? 
    (parse! number)
    (typed (any-number) (any-type))))

(check 
  (obj=? 
    (parse! string)
    (typed (any-string) (any-type))))

(check
  (obj=?
    (parse! (arrow number string))
    (typed (arrow (any-number) (any-string)) (any-type))))

; === any-tupleure make ===

(check
  (obj=?
    (parse! (foo 10 "bar"))
    (typed
      (tuple! 10 "bar")
      (any-tuple `foo (list (any-number) (any-string))))))

; === use / get ===

(check 
  (obj=?
    (parse! (use ("foo") (get string)))
    (typed
      (application! (abstraction 1 (variable 0)) "foo")
      (any-string))))

; === use / application ===

(check
  (obj=?
    (parse! (use ((native string-length (arrow (length string) number))) (length "foo")))
    (typed
      (application!
        (abstraction 1 (application! (variable 0) "foo"))
        (native `string-length))
      (any-number))))

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
    (typed "3 apples" (any-string))))
