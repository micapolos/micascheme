(import (check) (labs macro) (micascheme))

(run-void
  (define-syntax-literal? +)
  (define-syntax-literal? -)

  (check
    (equal?
      (syntax->datum
        (macro-case #'(+ "foo" "bar")
          ((+ a b) #'(a plus b))
          ((- a b) #'(a minus b))
          ((op a b) #'(a op b))))
      '("foo" plus "bar")))

  (check
    (equal?
      (syntax->datum
        (macro-case #'(- "foo" "bar")
          ((+ a b) #'(a plus b))
          ((- a b) #'(a minus b))
          ((op a b) #'(a op b))))
      '("foo" minus "bar")))

  (check
    (equal?
      (syntax->datum
        (macro-case #'(* "foo" "bar")
          ((+ a b) #'(a plus b))
          ((- a b) #'(a minus b))
          ((op a b) #'(a op b))))
      '("foo" * "bar")))
)

(run-void
  (define-syntax-literal? +)
  (define-syntax-literal? -)

  (define-syntax-matcher-2 string
    (lambda ($pattern)
      (syntax-case $pattern (string)
        ((string $string)
          (identifier? #'$string)
          (lambda ($body)
            #`(lambda ($syntax)
              (and
                (string? (syntax->datum $syntax))
                (with-syntax (($string $syntax)) #,$body))))))))

  (check
    (equal?
      (syntax->datum
        (macro-case-opt-2 #'(+ "foo" "bar")
          ((+ a b) #'(a plus b))
          ((- a b) #'(a minus b))
          ((op a b) #'(a op b))))
      '("foo" plus "bar")))

  (check
    (equal?
      (syntax->datum
        (macro-case-opt-2 #'(- "foo" "bar")
          ((+ a b) #'(a plus b))
          ((- a b) #'(a minus b))
          ((op a b) #'(a op b))))
      '("foo" minus "bar")))

  (check
    (equal?
      (syntax->datum
        (macro-case-opt-2 #'(* "foo" "bar")
          ((+ a b) #'(a plus b))
          ((- a b) #'(a minus b))
          ((op a b) #'(a op b))))
      '("foo" * "bar")))

  (check
    (false?
      (macro-case-opt-2 #'"foo"
        ((+ a b) #'(a plus b))
        ((- a b) #'(a minus b))
        ((op a b) #'(a op b)))))

  (check
    (equal?
      (macro-case-opt-2 #'"foo"
        ((string $string) (string-append (datum $string) "!")))
      "foo!"))

  (check
    (false?
      (macro-case-opt-2 #'123
        ((string $string)
          (string-append (datum $string) "!")))))

  (check
    (equal?
      (macro-case-opt-2 #'(+ "foo" "bar")
        ((+ (string $string1) (string $string2))
          (string-append (datum $string1) (datum $string2))))
      "foobar"))

  (check
    (false?
      (macro-case-opt-2 #'(+ "foo" 10)
        ((+ (string $string1) (string $string2))
          (string-append (datum $string1) (datum $string2))))))

  (check
    (false?
      (macro-case-opt-2 #'(+ 10 "foo")
        ((+ (string $string1) (string $string2))
          (string-append (datum $string1) (datum $string2))))))
)

(run-void
  (define-syntax-matcher-2 (string $pattern)
    (syntax-case $pattern ()
      ((_ $string)
        (lambda ($body)
          #`(lambda ($syntax)
            (and
              (string? (syntax->datum $syntax))
              (with-syntax (($string $syntax))
                #,$body)))))))

  (define-syntax-matcher-2 (number $pattern)
    (syntax-case $pattern ()
      ((_ $number)
        (lambda ($body)
          #`(lambda ($syntax)
            (and
              (number? (syntax->datum $syntax))
              (with-syntax (($number $syntax))
                #,$body)))))))

  (check
    (equal?
      (syntax->datum
        (macro-case-opt-2 #'"foo"
          ((string $string) #`(string-append $string "!"))
          ((number $number) #`(+ $number 1))))
      '(string-append "foo" "!")))

  (check
    (equal?
      (syntax->datum
        (macro-case-opt-2 #'123
          ((string $string) #`(string-append $string "!"))
          ((number $number) #`(+ $number 1))))
      '(+ 123 1)))

  (check
    (false?
      (macro-case-opt-2 #'#\space
        ((string $string) #`(string-append $string "!")))))
)

(run-void
  (define-macro join
    ((_ a b) (string-append a b))
    ((_ a b c) (string-append a b c)))

  (check (equal? (join "foo" "bar") "foobar"))
  (check (equal? (join "foo" "bar" "goo") "foobargoo"))
)

(run-void
  (define-macros
    ((join a b) (string-append a b))
    ((join a b c) (string-append a b c))
    ((dot-separate a b) (string-append a "." b)))

  (check (equal? (join "foo" "bar") "foobar"))
  (check (equal? (join "foo" "bar" "goo") "foobargoo"))
  (check (equal? (dot-separate "foo" "bar") "foo.bar"))
)
