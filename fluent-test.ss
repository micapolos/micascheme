(import
  (scheme)
  (check)
  (fluent)
  (list)
  (procedure))

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

(let ()
  (define $consumed (list))
  (define (consume . $values)
    (set! $consumed (append $values $consumed)))
  (check-equal?
    (fluent
      "foo"
      (also (consume "bar"))
      (string-append "goo"))
    "foogoo")
  (check-equal? $consumed (list "foo" "bar")))

(let ()
  (define $consumed (list))
  (define (consume . $values)
    (set! $consumed (append $values $consumed)))
  (check-equal?
    (fluent
      (3 (values "a" "b" "c"))
      (also (consume "d"))
      (string-append "d"))
    "abcd")
  (check-equal? $consumed (list "a" "b" "c" "d")))
