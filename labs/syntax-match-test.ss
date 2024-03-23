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

(define-syntax macro
  (lambda ($syntax)
    (syntax-case $syntax ()
      ((_ $entry ...)
        #`(lambda ($syntax)
          (lambda ($lookup)
            (or
              (syntax-match $lookup $syntax
                (list #'$entry ...))
              (syntax-error $syntax))))))))

(define-syntax define-macro
  (lambda ($syntax)
    (syntax-case $syntax ()
      ((_ $name $entry ...)
        #`(define-syntax $name
          (macro $entry ...))))))

(define-macro dawaj
  ((_ $a + $b) (string-append $a " + " $b))
  ((_ $a - $b) (string-append $a " - " $b))
  ((_ $a $op $b) (string-append $a " " $op " " $b))
  (_ "dawaj"))

(check (equal? (dawaj "foo" + "bar") "foo + bar"))
(check (equal? (dawaj "foo" - "bar") "foo - bar"))
(check (equal? (dawaj "foo" "*" "bar") "foo * bar"))
(check (equal? dawaj "dawaj"))

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

(check (equal? (syntax->datum (match-ref (match (a 10) (b 20)) #'a)) 10))
(check (equal? (syntax->datum (match-ref (match (a 10) (b 20)) #'b)) 20))
(check (equal? (syntax->datum (match-ref (match (a 10) (b 20)) #'c)) #f))

(define-aux-keywords r b hl)

(define-syntax-matcher r
  (lambda ($lookup $syntax $pattern)
    (syntax-case $pattern ()
      ((_ $code)
        (and (identifier? #'$code))
        (syntax-case $syntax (b hl)
          (b (match ($code #b000)))
          ((hl) (match ($code #b110))))))))

(define-macro ld
  ((ld (r $r1) (r $r2))
    (list #b01 $r1 $r2))
  ((ld (r $r) $n)
    (list #b01 $r #b110 $n)))

(check
  (equal?
    (ld b (hl))
    (list #b01 #b000 #b110)))

; (check
;   (equal?
;     (ld (hl) 12)
;     (list #b01 #b110 #b110 12)))

(let ()
  (define-syntax define-macros
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $rule ...)
          (lets
            ($groups
              (fold-left
                (lambda ($groups $rule)
                  (assid-update-new
                    (syntax-rule-id $rule)
                    (partial cons $rule)
                    (lambda () (list $rule))
                    $groups))
                (list)
                (syntax->list #'($rule ...))))
            #`(begin
              #,@(map
                (lambda ($group)
                  #`(define-macro
                    #,(car $group)
                    #,@(reverse (cdr $group))))
                (reverse $groups))))))))

  (define-syntax-matcher string
    (lambda ($lookup $syntax $pattern)
      (syntax-case $pattern ()
        ((_ $string)
          (and (identifier? #'$string))
          (and
            (string? (syntax->datum $syntax))
            (match-put null-match #'$string $syntax))))))

  (define-macros
    ((plus (string a) (string b)) (string-append a b))
    ((plus a b) (+ a b))
    ((minus a b) (- a b))
    ((minus a) (- a)))

  (check (equal? (plus 3 2) 5))
  (check (equal? (plus "foo" "bar") "foobar"))
  (check (equal? (minus 3 2) 1))
  (check (equal? (minus 3) -3))
)
