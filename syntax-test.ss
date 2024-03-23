(import (check) (syntax) (procedure))

(define-aux-keyword foo)
(check (raises? (lambda () foo)))

(check (syntax-null? #'()))
(check (not (syntax-null? #'(1))))

(define (predicate-syntax s)
  (datum->syntax #'+
    (string->symbol
      (string-append
        (symbol->string s)
        "?"))))

(define-namespace matcher)
(define-matcher x "matcher-x")
(define-matcher y "matcher-y")
(check (equal? (matcher x) "matcher-x"))
(check (equal? (matcher y) "matcher-y"))
;(check (equal? (matcher z) #f)) => syntax error

; === syntax-map-identifiers ===

(let ()
  (define-aux-keyword original)
  (define-aux-keyword mapped)

  (define-syntax check-maps?
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $syntax $expected)
          #`(check
            (equal?
              (syntax->datum
                (syntax-map-identifiers
                  (lambda ($id)
                    (or
                      (and (free-identifier=? $id #'original) #'mapped)
                      $id))
                  #'$syntax))
              '$expected))))))

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
)
