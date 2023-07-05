(import 
  (micascheme) 
  (type))

(check (equal? (term->datum `foo) `foo))

(check (equal? (term->datum (abstraction `foo)) `(lambda (v0) foo)))
(check (equal? (term->datum (abstraction (variable 0))) `(lambda (v0) v0)))
(check (equal? (term->datum (abstraction (abstraction (variable 0)))) `(lambda (v0) (lambda (v1) v1))))
(check (equal? (term->datum (abstraction (abstraction (variable 1)))) `(lambda (v0) (lambda (v1) v0))))

(check (equal? (term->datum (application `foo `bar)) `(foo bar)))

; --------------------------------------------------------------

(check (matches? `foo `foo))
(check (not (matches? `foo `bar)))

(check (matches? (abstraction `foo) (abstraction `foo)))
(check (not (matches? (abstraction `foo) (abstraction `bar))))

(check (matches? (abstraction `foo) `foo))
(check (not (matches? (abstraction `foo) `bar)))

(check (matches? (abstraction (variable 0)) `foo))
(check (matches? (abstraction (arrow (variable 0) (variable 0))) (arrow `foo `foo)))
(check (not (matches? (abstraction (arrow (variable 0) (variable 0))) (arrow `foo `bar))))

(check (matches? (abstraction (abstraction (arrow (variable 0) (variable 1)))) (arrow `foo `foo)))
(check (matches? (abstraction (abstraction (arrow (variable 0) (variable 1)))) (arrow `foo `bar)))

(check (matches? (abstraction (abstraction (variable 0))) (abstraction (abstraction (variable 0)))))
(check (not (matches? (abstraction (abstraction (variable 0))) (abstraction (abstraction (variable 1))))))

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
