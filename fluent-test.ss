(import
  (scheme)
  (check)
  (fluent))

(check (equal? (fluent "foo") "foo"))

(check
  (equal?
    (fluent
      "foo"
      (string-append "bar")
      (string-length))
    6))
