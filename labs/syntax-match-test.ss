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
          (syntax-match $lookup #'$syntax (syntax->list #'($entry ...))))))))

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
