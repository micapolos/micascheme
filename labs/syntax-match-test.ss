(import (check) (labs syntax-match) (micascheme))

; === parse-pattern ===

(let ()
  (define ($lookup $key $id)
    (and
      (free-identifier=? $id #'pattern-matcher)
      (free-identifier=? $key #'matcher)
      (lambda ($syntax)
        (syntax-case $syntax ()
          ($id
            (identifier? #'$id)
            #'matched)
          (($id $a $b)
            (identifier? #'$id)
            #'(matched $a $b))))))

  (check
    (equal?
      (syntax->datum (parse-pattern $lookup #'$foo))
      '($foo . ())))

  (check
    (equal?
      (with-generate-temporary-seed $tmp
        (syntax->datum
          (parse-pattern $lookup
            #'matcher)))
      '($tmp-0 . (($tmp-0 . matched)))))

  (check
    (equal?
      (with-generate-temporary-seed $tmp
        (syntax->datum
          (parse-pattern $lookup
            #'(matcher $a $b))))
      '($tmp-0 . (($tmp-0 . (matched $a $b))))))

  (check
    (equal?
      (with-generate-temporary-seed $tmp
        (syntax->datum
          (parse-pattern $lookup
            #'(foo $bar matcher (matcher $a $b)))))
      '((foo $bar $tmp-0 $tmp-1) . (($tmp-0 . matched) ($tmp-1 . (matched $a $b))))))
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
