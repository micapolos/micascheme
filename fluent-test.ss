(import
  (check)
  (fluent))

(check (equal? (fluent #f) #f))
(check (equal? (fluent 128) 128))
(check (equal? (fluent "foo") "foo"))
(check (equal? (fluent 'foo) 'foo))
(check (equal? (fluent (values +)) +))

(check (equal? (call-with-values (lambda () (fluent)) list) (list)))
(check (equal? (call-with-values (lambda () (fluent "foo" "bar")) list) (list "foo" "bar")))

(check (equal? (fluent string-append) (string-append)))
(check (equal? (fluent (string-append)) (string-append)))
(check (equal? (fluent (string-append "a")) (string-append "a")))
(check (equal? (fluent (string-append "a" "b")) (string-append "a" "b")))

(check (equal? (fluent "a" string-append) (string-append "a")))

(check (equal? (fluent "a" string-append) (string-append "a")))
(check (equal? (fluent "a" (string-append)) (string-append "a")))
(check (equal? (fluent "a" (string-append "b")) (string-append "a" "b")))
(check (equal? (fluent "a" (string-append "b" "c")) (string-append "a" "b" "c")))

(check (equal? (fluent "a" "b" string-append) (string-append "a" "b")))
(check (equal? (fluent "a" "b" (string-append)) (string-append "a" "b")))
(check (equal? (fluent "a" "b" (string-append "c")) (string-append "a" "b" "c")))
(check (equal? (fluent "a" "b" (string-append "c" "d")) (string-append "a" "b" "c" "d")))

(check (equal? (fluent "a" (fluent 3 (- 2)) cons) (cons "a" (- 3 2))))

(check (equal? (fluent "a" (values "b" (string-append "c" "d")) string-append) (string-append "a" "b" (string-append "c" "d"))))
