(import
  (check)
  (fluent)
  (procedure))

(check (equal? (fluent #f) #f))
(check (equal? (fluent 128) 128))
(check (equal? (fluent "foo") "foo"))
(check (equal? (fluent 'foo) 'foo))
(check (equal? (fluent +) +))

(check (equal? (call-with-values (lambda () (fluent)) list) (list)))
(check (equal? (call-with-values (lambda () (fluent "foo" "bar")) list) (list "foo" "bar")))

(check (equal? (fluent string-append) string-append))
(check (equal? (fluent (string-append)) (string-append)))
(check (equal? (fluent (string-append "a")) (string-append "a")))
(check (equal? (fluent (string-append "a" "b")) (string-append "a" "b")))

(check (equal? (fluent "a" (string-append)) (string-append "a")))

(check (equal? (fluent "a" (string-append)) (string-append "a")))
(check (equal? (fluent "a" (string-append "b")) (string-append "a" "b")))
(check (equal? (fluent "a" (string-append "b" "c")) (string-append "a" "b" "c")))

(check (equal? (fluent "a" "b" (string-append)) (string-append "a" "b")))
(check (equal? (fluent "a" "b" (string-append "c")) (string-append "a" "b" "c")))
(check (equal? (fluent "a" "b" (string-append "c" "d")) (string-append "a" "b" "c" "d")))

(check
  (equal?
    (fluent
      "a"
      (fluent 3 (- 2))
      (cons))
    (cons "a" (- 3 2))))

(check
  (equal?
    (fluent
      "a"
      (values "b" (string-append "c" "d"))
      (string-append))
    (string-append "a" "b" (string-append "c" "d"))))

(check
  (equal?
    (fluent
      "foo"
      (let $string
        $string
        ", "
        $string
        (string-append)))
    (let (($string "foo"))
      (string-append $string ", " $string))))

(check
  (equal?
    (fluent
      "foo"
      "bar"
      (let (values $string-1 $string-2)
        $string-1
        ", "
        $string-2
        (string-append)))
    (call-with-values
      (lambda () (values "foo" "bar"))
      (lambda ($string-1 $string-2)
        (string-append $string-1 ", " $string-2)))))

(check
  (equal?
    (fluent
      $a
      $b
      (lambda
        $a
        $b
        (string-append))
      (app "a" "b"))
    (app
      (lambda ($a $b) (string-append $a $b))
      "a" "b")))

(check
  (equal?
    (fluent
      "Hello, "
      (string-append "world!")
      (let $string
        $string
        " ("
        (fluent
          $string
          (string-length)
          (number->string))
        ")"
        (string-append)))
    (let (($string (string-append "Hello, " "world!")))
      (string-append
        $string
        " ("
        (number->string (string-length $string))
        ")"))))
