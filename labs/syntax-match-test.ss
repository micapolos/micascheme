(import (check) (labs syntax-match) (micascheme))

; === syntax-match-apply ===

(let ()
  (define-aux-keyword original)
  (define-aux-keyword mapped)

  (define-syntax-rule (check-maps? $syntax $expected)
    (check
      (equal?
        (syntax->datum
          (syntax-match-apply
            (lambda ($id)
              (if (free-identifier=? $id #'original) #'mapped $id))
            #'$syntax))
        '$expected)))

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

(define-syntax syntax-match!
  (lambda ($syntax)
    (lambda ($lookup)
      (syntax-case $syntax ()
        ((_ $syntax $entry ...)
          (or
            (syntax-match $lookup #'$syntax (syntax->list #'($entry ...)))
            (syntax-error #'$syntax)))))))

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
  (procedure?
    (pattern-rules
      ((r $code)
        (b #b000)
        (c #b001)
        (d #b010)
        (e #b011)
        (h #b100)
        (l #b101)
        (a #b111))
      ((r $prefix $code)
        (ixh #xdd #b100)
        (ixl #xdd #b101)
        (iyh #xfd #b100)
        (iyl #xfd #b101))
      ((r $prefix $code $offset)
        ((+ ix $d) #xdd #b110 $d)
        ((- ix $d) #xdd #b110 $d)
        ((+ ix $d) #xdd #b110 $d)
        ((- iy $d) #xfd #b110 $d)))))

