(import 
  (micascheme) 
  (term)
  (type))

; --------------------------------------------------------------

(check (matches? `foo `foo))
(check (not (matches? `foo `bar)))

(check (matches? (abstraction 2 `foo) (abstraction 2`foo)))
(check (not (matches? (abstraction 2 `foo) (abstraction 3 `foo))))
(check (not (matches? (abstraction 2 `foo) (abstraction 2 `bar))))

(check (matches? (abstraction 1 `foo) `foo))
(check (not (matches? (abstraction 1 `foo) `bar)))

(check (matches? (abstraction 2 (variable 0)) `foo))
(check (matches? (abstraction 2 (arrow (variable 0) (variable 0))) (arrow `foo `foo)))
(check (not (matches? (abstraction 1 (arrow (variable 0) (variable 0))) (arrow `foo `bar))))

(check (matches? (abstraction 2 (arrow (variable 0) (variable 1))) (arrow `foo `foo)))
(check (matches? (abstraction 2 (arrow (variable 0) (variable 1))) (arrow `foo `bar)))

(check (matches? (abstraction 2 (variable 0)) (abstraction 2 (variable 0))))
(check (not (matches? (abstraction 2 (variable 0)) (abstraction 2 (variable 1)))))

(check 
  (obj=?
    (parse
      (list 
        (arrow `(length ,(any-string)) (any-number))
        (arrow `(string ,(any-number)) (any-string))
        (arrow `(append ,(any-string) ,(any-string)) (any-string)))
      #`(append (string (length "foo")) " apples"))
    (typed 
      `(v2 (v1 (v0 "foo")) " apples")
      (any-string))))

(check
  (obj=?
    (evaluate
      (list 
        (cons `string-length (arrow `(length ,(any-string)) (any-number)))
        (cons `number->string (arrow `(string ,(any-number)) (any-string)))
        (cons `string-append (arrow `(append ,(any-string) ,(any-string)) (any-string))))
      #`(append (string (length "foo")) " apples"))
    (typed "3 apples" (any-string))))
