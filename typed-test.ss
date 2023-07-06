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

; === structure make ===

(check
  (obj=?
    (parse! (foo 10 "bar"))
    (typed
      (make-tuple (list (any-number) (any-string)) (list 10 "bar"))
      (any-tuple `foo (list (any-number) (any-string))))))

; === type make ===

(check
  (obj=?
    (parse! (type (foo number string)))
    (typed
      (any-tuple `foo (list (any-number) (any-string)))
      (any-type))))

; === application ===

(check 
  (obj=?
    (env-parse
      (list 
        (arrow (any-tuple `length (list (any-string))) (any-number))
        (arrow (any-tuple `string (list (any-number))) (any-string))
        (arrow (any-tuple `append (list (any-string) (any-string))) (any-string)))
      #f
      #`(append (string (length "foo")) " apples"))
    (typed
      (application! (variable 2)
        (application! (variable 1) 
          (application! (variable 0) "foo"))
        " apples")
      (any-string))))

; === let / get ===

(check 
  (obj=?
    (parse! (let ("foo") (get string)))
    (typed
      (application! (abstraction 1 (variable 0)) "foo")
      (any-string))))

; === evaluate ===

(check (obj=? (evaluate (list) #`foo) (typed #f `foo)))

; (check
;   (obj=?
;     (evaluate
;       (list (cons `real-time (arrow (any-tuple `time (list)) (any-number))))
;       #`(time))
;     (typed 12 (any-number))))

; (check
;   (obj=?
;     (evaluate
;       (list
;         (cons `string-length (arrow (any-tuple `length (list (any-string))) (any-number)))
;         (cons `number->string (arrow (any-tuple `string (list (any-number))) (any-string)))
;         (cons `string-append (arrow (any-tuple `append (list (any-string) (any-string))) (any-string))))
;       #`(append (string (length "foo")) " apples"))
;     (typed "3 apples" (any-string))))
