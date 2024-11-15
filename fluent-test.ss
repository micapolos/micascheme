(import
  (scheme)
  (check)
  (fluent)
  (list))

(check (equal? (fluent "foo") "foo"))

(check
  (equal?
    (fluent
      "foo"
      (string-append "bar")
      (string-length))
    6))

(check
  (equal?
    (fluent
      "foo"
      (string-append "bar")
      (with $it (string-append "Hello, " $it))
      (string-append "!"))
    "Hello, foobar!"))

(check
  (equal?
    (values->list (fluent (values 1 2 3)))
    (list 1 2 3)))

(check
  (equal?
    (fluent
      (values "a" "b" "c")
      (string-append "d"))
    "abcd"))
