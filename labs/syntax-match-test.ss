(import (check) (labs syntax-match) (micascheme))

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

(check (equal? (syntax->datum ((match (a 10) (b 20)) #'a)) 10))
(check (equal? (syntax->datum ((match (a 10) (b 20)) #'b)) 20))
(check (equal? (syntax->datum ((match (a 10) (b 20)) #'c)) #f))

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
