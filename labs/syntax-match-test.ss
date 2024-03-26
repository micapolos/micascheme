(import (check) (labs syntax-match) (micascheme))

; === pattern-match ===

(run-void
  (define-syntax (pattern-match $syntax $lookup)
    (syntax-case $syntax ()
      ((_ $syntax $pattern $body)
        (parse-pattern-match $lookup #'$syntax #'$pattern #'$body))))

  (define-aux-keyword literal)

  (define-property literal pattern-literal? #t)

  (define-aux-keyword matcher)
  (define-aux-keyword match)

  (define-property matcher pattern-matcher
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

  ; === open ===

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

  ; === literal ===

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

  ; === matcher ===

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

; === syntax-match ===

(check
  (equal?
    (syntax->datum
      (syntax-match-apply-2
        (id-syntax-match #'foo #'(+ 1 2))
        #'(* foo 3)))
    '(syntax-case #'(+ 1 2) ()
      (foo (* foo 3)))))

; === syntax-clause-apply ===

(lets
  ($lookup
    (lambda ($key $id)
      (and
        (free-identifier=? $id #'syntax-literal?)
        (free-identifier=? $key #'+))))

  (run
    (check
      (equal?
        (syntax->datum
          (syntax-clause-apply
            $lookup
            #'(+ a b)
            #'((+ $a $b) body)))
        `(syntax-case #'(a b) ()
          (($a $b) #'body))))

    (check
      (equal?
        (syntax->datum
          (syntax-clause-apply
            $lookup
            #'(- a b)
            #'((+ $a $b) body)))
        #f))))

; === syntax-clauses-apply ===

(lets
  ($lookup
    (lambda ($key $id)
      (and
        (free-identifier=? $id #'syntax-literal?)
        (or
          (free-identifier=? $key #'+)
          (free-identifier=? $key #'-)))))
  ($clauses
    (list
      #'((+ $a $b) body1)
      #'((- $a $b) body2)
      #'(($op $a $b) body3)
      #'($other body4)))

  (run
    (check
      (equal?
        (syntax->datum
          (syntax-clauses-apply
            $lookup
            #'(+ a b)
            $clauses))
        `(syntax-case #'(a b) ()
          (($a $b) #'body1))))

    (check
      (equal?
        (syntax->datum
          (syntax-clauses-apply
            $lookup
            #'(- a b)
            $clauses))
        `(syntax-case #'(a b) ()
          (($a $b) #'body2))))

    (check
      (equal?
        (syntax->datum
          (syntax-clauses-apply
            $lookup
            #'(* a b)
            $clauses))
        `(syntax-case #'(* a b) ()
          (($op $a $b) #'body3))))

    (check
      (equal?
        (syntax->datum
          (syntax-clauses-apply
            $lookup
            #'other
            $clauses))
        `(syntax-case #'(other) ()
          (($other) #'body4))))))

; === syntax-match-apply ===

(let ()
  (define-aux-keyword original)
  (define-aux-keyword mapped)

  (define-syntax (check-maps? $syntax)
    (syntax-case $syntax ()
      ((_ $syntax $expected)
        #`(check
          (equal?
            '#,(syntax-match-apply
              (id-match #'original #'mapped)
              #'$syntax)
            '$expected)))))

  (check-maps? original mapped)
  (check-maps? other other)

  (check-maps? #'original #'original)
  (check-maps? #'#,original #'#,original)

  (check-maps? #`original #`original)
  (check-maps? #`#,original #`#,mapped)

  (check-maps? #`#,#'original #`#,#'original)
  (check-maps? #`#'#,original #`#'#,mapped)
  (check-maps? #'#`#,original #'#`#,original)

  (check-maps? #`#`#,original #`#`#,original)
  (check-maps? #`#`#,#,original #`#`#,#,mapped)

  (check-maps? #`#,@(original original) #`#,@(mapped mapped))
  (check-maps? #`#`#,@(original original) #`#`#,@(original original))
  (check-maps? #`#`#,@#,@(original original) #`#`#,@#,@(mapped mapped))

  (check-maps? #,#'original #,#'mapped)
  (check-maps? #,#`original #,#`mapped)

  (check-maps? #,#,#'original #,#,#'original)
  (check-maps? #,#,#'#'original #,#,#'#'mapped)

  (check-maps? (original original) (mapped mapped))
  (check-maps? (original other) (mapped other))

  (check-maps? (original syntax original) (mapped syntax mapped))
  (check-maps? (original quasisyntax original) (mapped quasisyntax mapped))
  (check-maps? (original unsyntax original) (mapped unsyntax mapped))
  (check-maps? (original unsyntax-splicing original) (mapped unsyntax-splicing mapped))
)

(define-literal? +)
(define-literal? -)

(define-syntax (syntax-match! $syntax $lookup)
  (syntax-case $syntax ()
    ((_ $syntax $entry ...)
      (syntax-match $lookup #'$syntax (syntax->list #'($entry ...))))))

(check
  (equal?
    (syntax-match! (+ "foo" "bar")
      ((+ $a $b) (string-append $a " + " $b))
      ((- $a $b) (string-append $a " - " $b))
      (($op $a $b) (string-append $a " " $op " " $b)))
    "foo + bar"))

(check
  (equal?
    (syntax-match! (- "foo" "bar")
      ((+ $a $b) (string-append $a " + " $b))
      ((- $a $b) (string-append $a " - " $b))
      (($op $a $b) (string-append $a " " $op " " $b)))
    "foo - bar"))

(check
  (equal?
    (syntax-match! ("*" "foo" "bar")
      ((+ $a $b) (string-append $a " + " $b))
      ((- $a $b) (string-append $a " - " $b))
      (($op $a $b) (string-append $a " " $op " " $b)))
    "foo * bar"))

(check (equal? (syntax->datum (match-ref (match (a #'10) (b #'20)) #'a)) 10))
(check (equal? (syntax->datum (match-ref (match (a #'10) (b #'20)) #'b)) 20))
(check (equal? (syntax->datum (match-ref (match (a #'10) (b #'20)) #'c)) #f))

(check (equal? (syntax->datum (match-ref (combined-match (a b) #'10 #'20) #'a)) 10))
(check (equal? (syntax->datum (match-ref (combined-match (a b) #'10 #'20) #'b)) 20))
(check (equal? (syntax->datum (match-ref (combined-match (a b) #'10 #'20) #'c)) #f))

(check
  (equal?
    (syntax->datum
      (match-apply-syntax
        (match ($a #'a) ($b #'b))
        #'body))
    '(syntax-case #'(a b) ()
      (($a $b) #'body))))
