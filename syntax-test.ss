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

(define-syntax check-maps?
  (lambda ($syntax)
    (syntax-case $syntax ()
      ((_ $syntax $expected)
        #`(check
          (equal?
            #,(syntax-map-identifiers
              (lambda ($id)
                (or
                  (and (free-identifier=? $id #'+) #'string-append)
                  $id))
              #'$syntax)
            $expected))))))

(check-maps? + string-append)
(check-maps? - -)
(check-maps? (syntax->datum #'+) '+)
(check-maps? (syntax->datum #`+) '+)
(check-maps? (syntax->datum #`#`#,+) '#`#,+)
(check-maps? #`#,+ string-append)
(check-maps? (syntax->datum #`#`#,#,+) `#`#,,string-append)

(check
  (equal?
    (syntax->datum
      (syntax-map-identifiers
        (lambda ($identifier)
          (cond
            ((free-identifier=? $identifier #'+) #'string-append)
            (else $identifier)))
        #`(+ (+ "foo" "bar") "zoo" other)))
    `(string-append (string-append "foo" "bar") "zoo" other)))
