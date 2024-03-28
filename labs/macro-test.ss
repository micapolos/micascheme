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

  (check
    (equal?
      (syntax->datum
        (macro-case-opt-2 #'"foo"
          ((string $string) #`(string-append $string "!"))))
      '(string-append "foo" "!")))

  (check
    (false?
      (macro-case-opt-2 #'123
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
