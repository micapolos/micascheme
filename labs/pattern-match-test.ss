(import (check) (labs pattern-match) (micascheme))

; === pattern-match ===

(run-void
  (define-syntax (pattern-match $syntax $lookup)
    (syntax-case $syntax ()
      ((_ $syntax $pattern $body)
        (parse-pattern-clause $lookup #'$syntax #'($pattern $body)))))

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

; === parse-pattern-2 ===

(run-void
  (define ($lookup $key $id) #f)

  (check
    (equal?
      (syntax->datum
        (app
          (parse-pattern-2 $lookup #'120)
          #'match))
      '(lambda ($syntax)
        (and
          (equal? (syntax->datum $syntax) '120)
          match))))

  (check
    (equal?
      (syntax->datum
        (app
          (parse-pattern-2 $lookup #'$x)
          #'(match $x)))
      '(lambda ($syntax)
        (with-syntax (($x $syntax))
          (match $x)))))

  (check
    (equal?
      (syntax->datum
        (app
          (parse-pattern-2 $lookup #'())
          #'match))
      '(lambda ($syntax)
        (syntax-case-opt $syntax ()
          (() match)))))

  (check
    (equal?
      (syntax->datum
        (app
          (parse-pattern-2 $lookup #'($x))
          #'(match $x)))
      '(lambda ($syntax)
        (syntax-case-opt $syntax ()
          (($syntax-head . $syntax-tail)
            (app
              (app
                (lambda ($syntax)
                  (syntax-case-opt $syntax ()
                    (()
                      (lambda ($syntax)
                        (with-syntax (($x $syntax))
                          (match $x))))))
                #'$syntax-tail)
              #'$syntax-head))))))

  (check
    (equal?
      (syntax->datum
        (app
          (parse-pattern-2 $lookup #'($x $y))
          #'(match $x $y)))
      '(lambda ($syntax)
        (syntax-case-opt $syntax ()
          (($syntax-head . $syntax-tail)
            (app
              (app
                (lambda ($syntax)
                  (syntax-case-opt $syntax ()
                    (($syntax-head . $syntax-tail)
                      (app
                        (app
                          (lambda ($syntax)
                            (syntax-case-opt $syntax ()
                              (()
                                (lambda ($syntax)
                                  (with-syntax (($y $syntax))
                                    (lambda ($syntax)
                                      (with-syntax (($x $syntax))
                                        (match $x $y))))))))
                          #'$syntax-tail)
                        #'$syntax-head))))
                #'$syntax-tail)
              #'$syntax-head))))))
)

(run-void
  (define-aux-keyword literal)

  (define ($lookup $key $id)
    (and
      (free-identifier=? $id #'syntax-literal?)
      (free-identifier=? $key #'literal)))

  (check
    (equal?
      (syntax->datum
        (app
          (parse-pattern-2 $lookup #'literal)
          #'match))
      '(lambda ($syntax)
        (and
          (identifier? $syntax)
          (free-identifier=? $syntax #'literal)
          match))))
)

(run-void
  (define-aux-keyword matcher)

  (define ($lookup $key $id)
    (and
      (free-identifier=? $id #'syntax-matcher-2)
      (free-identifier=? $key #'matcher)
      (lambda ($pattern)
        (syntax-case $pattern ()
          ((_ $a)
            (lambda ($body)
              #`(match #,$body)))))))

  (check
    (equal?
      (syntax->datum
        (app
          (parse-pattern-2 $lookup #'(matcher $a))
          #'foo))
      '(match foo)))
)

(run-void
  (define-aux-keyword matcher)

  (define ($lookup $key $id)
    (and
      (free-identifier=? $id #'syntax-matcher-2)
      (free-identifier=? $key #'matcher)
      (lambda ($pattern)
        (syntax-case $pattern ()
          ((_ $a)
            (lambda ($body) #'#f))))))

  (check
    (equal?
      (syntax->datum
        (app
          (parse-pattern-2 $lookup #'(matcher $a))
          #'foo))
      #f))
)
