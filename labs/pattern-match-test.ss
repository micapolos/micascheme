(import (check) (labs pattern-match) (micascheme))

; === pattern-match ===

(run-void
  (define-syntax (pattern-match $syntax $lookup)
    (syntax-case $syntax ()
      ((_ $syntax $pattern $body)
        (parse-pattern-match $lookup #'$syntax #'$pattern #'$body))))

  (define-aux-keyword literal)

  (define-property literal syntax-literal? #t)

  (define-aux-keyword matcher)
  (define-aux-keyword match)

  (define-property matcher syntax-matcher
    (lambda ($pattern)
      (syntax-case $pattern ()
        ((_ $param1 $param2)
          (and
            (identifier? #'$param1)
            (identifier? #'$param2))
          (values
            (list #'$param1 #'$param2)
            #`(lambda ($syntax)
              (syntax-case-opt $syntax (match)
                ((match $arg1 $arg2) (list #'$arg1 #'$arg2)))))))))

  (check
    (equal?
      (syntax->datum
        (pattern-match
          #'("foo" "bar")
          ($a $b)
          #'(string-append $a $b)))
      '(string-append "foo" "bar")))

  (check
    (false?
      (pattern-match
        #'"foo"
        ($a $b)
        #'(string-append $a $b))))

  (check
    (equal?
      (syntax->datum
        (pattern-match #'literal literal #'matched))
      'matched))

  (check
    (equal?
      (syntax->datum
        (pattern-match
          #'(literal "foo" "bar")
          (literal $a $b)
          #'(string-append $a $b)))
      '(string-append "foo" "bar")))

  (check
    (false?
      (pattern-match #'not-literal literal #'matched)))

  (check
    (false?
      (pattern-match #'not-literal literal #'matched)))

  (check
    (equal?
      (syntax->datum
        (pattern-match
          #'(match "foo" "bar")
          (matcher $a $b)
          #'(string-append $a $b)))
      '(string-append "foo" "bar")))

  (check
    (false?
      (pattern-match
        #'(match "foo")
        (matcher $a $b)
        #'(string-append $a $b))))

  (check
    (false?
      (pattern-match
        #'(mismatch "foo" "bar")
        (matcher $a $b)
        #'(string-append $a $b))))
)
