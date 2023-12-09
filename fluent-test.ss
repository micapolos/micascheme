(import
  (scheme)
  (check)
  (fluent))

(check (equal? (fluent #f) #f))
(check (equal? (fluent 128) 128))
(check (equal? (fluent "foo") "foo"))
(check (equal? (fluent foo (quote)) 'foo))
(check (equal? (fluent +) +))

(check (equal? (call-with-values (lambda () (fluent)) list) (list)))
(check (equal? (call-with-values (lambda () (fluent "foo" "bar")) list) (list "foo" "bar")))

(check (equal? (fluent string-append) string-append))
(check (equal? (fluent (string-append)) ""))
(check (equal? (fluent (string-append "a")) "a"))

(check (equal? (fluent "a" (string-append)) "a"))

(check (equal? (fluent "a" (string-append)) "a"))
(check (equal? (fluent "a" (string-append "b")) "ab"))

(check (equal? (fluent "a" "b" (string-append)) "ab"))
(check (equal? (fluent "a" "b" (string-append "c")) "abc"))
(check (equal? (fluent "a" "b" (string-append "c" (string-append "d"))) "abcd"))

; === values

(check
  (equal?
    (fluent
      "a"
      (values "b" (string-append "c" "d"))
      (string-append))
    "abcd"))

; === lambda

(check
  (equal?
    (
      (fluent
        $a $b
        (lambda $a (string-append $b)))
      "a" "b")
    "ab"))

; === apply

(check
  (equal?
    (fluent
      string-append
      (apply "a" "b" "c"))
    "abc"))

(check
  (equal?
    (fluent
      string-append
      (apply))
    ""))

; === let

(check
  (equal?
    (fluent
      "foo"
      (let $string
        $string
        ", "
        $string
        (string-append)))
    "foo, foo"))

(check
  (equal?
    (fluent
      "foo"
      (let $string
        $string
        ", "
        $string
        (string-append)))
    "foo, foo"))

(check
  (equal?
    (fluent
      "foo" "bar"
      (let (values $string-1 $string-2)
        $string-1
        ", "
        $string-2
        (string-append)))
    "foo, bar"))

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
    "Hello, world! (13)"))

; === define

(check
  (equal?
    (fluent
      (define $foo "foo")
      (define $bar "bar")
      (define $foobar
        $foo
        ", "
        $bar
        (string-append))
      $foobar)
    "foo, bar"))

(check
  (equal?
    (fluent
      "("
      (define $foo "f" "o" "o" (string-append))
      $foo
      ", "
      $foo
      ")"
      (string-append))
    "(foo, foo)"))

(check
  (equal?
    (fluent
      "("
      (define (values $foo $bar)
        (fluent "foo")
        (fluent "bar"))
      $foo
      ", "
      $bar
      ")"
      (string-append))
    "(foo, bar)"))
