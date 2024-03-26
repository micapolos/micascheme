(import (check) (labs macro) (labs syntax-match) (micascheme))

(run-void
  (define-pattern-literal? +)
  (define-pattern-literal? -)

  (check
    (equal?
      (syntax->datum
        (macro-case-2 #'(+ "foo" "bar")
          ((+ a b) #'(a plus b))
          ((- a b) #'(a minus b))
          ((op a b) #'(a op b))))
      '("foo" plus "bar")))

  (check
    (equal?
      (syntax->datum
        (macro-case-2 #'(- "foo" "bar")
          ((+ a b) #'(a plus b))
          ((- a b) #'(a minus b))
          ((op a b) #'(a op b))))
      '("foo" minus "bar")))

  (check
    (equal?
      (syntax->datum
        (macro-case-2 #'(* "foo" "bar")
          ((+ a b) #'(a plus b))
          ((- a b) #'(a minus b))
          ((op a b) #'(a op b))))
      '("foo" * "bar")))
)

(run-void
  (define-macro-2 join
    ((_ a b) (string-append a b))
    ((_ a b c) (string-append a b c)))

  (check (equal? (join "foo" "bar") "foobar"))
  (check (equal? (join "foo" "bar" "goo") "foobargoo"))
)

(run-void
  (define-macros-2
    ((join a b) (string-append a b))
    ((join a b c) (string-append a b c))
    ((dot-separate a b) (string-append a "." b)))

  (check (equal? (join "foo" "bar") "foobar"))
  (check (equal? (join "foo" "bar" "goo") "foobargoo"))
  (check (equal? (dot-separate "foo" "bar") "foo.bar"))
)
