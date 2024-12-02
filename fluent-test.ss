(import
  (scheme)
  (check)
  (fluent)
  (list))

(check-equal?
  (fluent)
  (void))

(check-equal?
  (fluent "foo")
  "foo")

(check-equal?
  (fluent
    "foo"
    (string-append "bar")
    (string-length))
  6)

(check-equal?
  (fluent
    "foo"
    (string-append "bar")
    (let $it (string-append "Hello, " $it))
    (string-append "!"))
  "Hello, foobar!")

(check-equal?
  (fluent
    (0 (values))
    (string-append "a" "b" "c"))
  "abc")

(check-equal?
  (fluent
    (3 (values "a" "b" "c"))
    (string-append "d"))
  "abcd")

(check-equal?
  (fluent
    (values)
    (let () (string-append "a" "b" "c")))
  "abc")

(check-equal?
  (fluent
    (values "a" "b" "c")
    (let ($a $b $c) (string-append $a $b $c "d")))
  "abcd")
