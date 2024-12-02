(import
  (scheme)
  (check)
  (fluent)
  (list)
  (procedure))

(check-equal?
  (values->list (fluent))
  (list))

(check-equal?
  (fluent "a")
  "a")

(check-equal?
  (fluent
    "a"
    (string-append "b"))
  "ab")

(check-equal?
  (fluent
    "a"
    (string-append "b")
    (string-length))
  2)

(check-equal?
  (fluent
    "c"
    (let $it (string-append "a" "b" $it))
    (string-append "d"))
  "abcd")

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
      "a"
      (also (consume "b"))
      (string-append "c"))
    "ac")
  (check-equal? $consumed (list "a" "b")))

 (let ()
  (define $consumed (list))
  (define (consume . $values)
    (set! $consumed (append $values $consumed)))
  (check-equal?
    (fluent
      (3 (values "a" "b" "c"))
      (also (consume "d"))
      (string-append "e"))
    "abce")
  (check-equal? $consumed (list "a" "b" "c" "d")))
