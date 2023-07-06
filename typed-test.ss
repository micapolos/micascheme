(import 
  (micascheme) 
  (term)
  (type) 
  (typed))

; === literals ===

(check 
  (obj=? 
    (parse (list) #`#t)
    (typed #t (any-boolean))))

(check 
  (obj=? 
    (parse (list) #`123)
    (typed 123 (any-number))))

(check 
  (obj=? 
    (parse (list) #`"foo")
    (typed "foo" (any-string))))

(check 
  (obj=? 
    (parse (list) #`foo)
    (typed (application! `quote `foo) `foo)))

; === native ===

(check
  (obj=?
    (parse (list) #`(native pi number))
    (typed (native `pi) (any-number))))

; === types ===

(check 
  (obj=? 
    (parse (list) #`boolean)
    (typed (any-boolean) (any-type))))

(check 
  (obj=? 
    (parse (list) #`number)
    (typed (any-number) (any-type))))

(check 
  (obj=? 
    (parse (list) #`string)
    (typed (any-string) (any-type))))

(check
  (obj=?
    (parse (list) #`(arrow number string))
    (typed (arrow (any-number) (any-string)) (any-type))))

; === structure make ===

(check
  (obj=?
    (parse (list) #`(foo 10 "bar"))
    (typed
      (make-tuple (list (any-number) (any-string)) (list 10 "bar"))
      `(foo ,(any-number) ,(any-string)))))

; === application ===

(check 
  (obj=?
    (parse
      (list 
        (arrow `(length ,(any-string)) (any-number))
        (arrow `(string ,(any-number)) (any-string))
        (arrow `(append ,(any-string) ,(any-string)) (any-string)))
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
    (parse (list) #`(let ("foo") (get string)))
    (typed
      (application! (abstraction 1 (variable 0)) "foo")
      (any-string))))

; === evaluate ===

; (check (obj=? (evaluate (list) #`foo) (typed `foo `foo)))

; (check
;   (obj=?
;     (evaluate
;       (list 
;         (cons `string-length (arrow `(length ,(any-string)) (any-number)))
;         (cons `number->string (arrow `(string ,(any-number)) (any-string)))
;         (cons `string-append (arrow `(append ,(any-string) ,(any-string)) (any-string))))
;       #`(append (string (length "foo")) " apples"))
;     (typed "3 apples" (any-string))))
