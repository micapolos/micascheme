(import (base) (base-syntax))

(check (equal? (index-switch 0 "zero" "one" "two") "zero"))
(check (equal? (index-switch 1 "zero" "one" "two") "one"))
(check (equal? (index-switch 2 "zero" "one" "two") "two"))
(check (equal? (index-switch 3 "zero" "one" "two") "two"))

; === define-struct ===

(define-struct (struct0))
(define-struct (struct1 string))
(define-struct (struct2 string number))
(define-struct (struct3 string number struct0))

(check (equal? (struct0) #f))
(check (equal? (struct1 "foo") "foo"))
(check (equal? (struct2 "foo" 128) (cons "foo" 128)))
(check (equal? (struct3 "foo" 128 (struct0)) (vector "foo" 128 (struct0))))

(check (equal? (struct1-string (struct1 "foo")) "foo"))
(check (equal? (struct2-string (struct2 "foo" 128)) "foo"))
(check (equal? (struct2-number (struct2 "foo" 128)) 128))
(check (equal? (struct3-string (struct3 "foo" 128 (struct0))) "foo"))
(check (equal? (struct3-number (struct3 "foo" 128 (struct0))) 128))
(check (equal? (struct3-struct0 (struct3 "foo" 128 (struct0))) (struct0)))

(check (equal? (struct0->datum (struct0)) `(struct0)))
(check (equal? (struct1->datum (struct1 "foo")) `(struct1 "foo")))
(check (equal? (struct2->datum (struct2 "foo" 128)) `(struct2 "foo" 128)))
(check (equal? (struct3->datum (struct3 "foo" 128 (struct0))) `(struct3 "foo" 128 (struct0))))

; === define-one-of ===

(define-one-of (one-of-3 string number struct0))

(check (equal? (one-of-3 "foo" (not number) (not struct0)) (cons 0 "foo")))
(check (equal? (one-of-3 (not string) 128 (not struct0)) (cons 1 128)))
(check (equal? (one-of-3 (not string) (not number) (struct0)) (cons 2 (struct0))))

(check 
  (equal? 
    (one-of-3-switch (one-of-3 "foo" (not number) (not struct0))
      ((string? $string) (string-append "string " $string))
      ((number? $number) (string-append "number " (number->string $number)))
      ((struct0? $struct0) "struct0"))
    "string foo"))

(check 
  (equal? 
    (one-of-3-switch (one-of-3 (not string) 128 (not struct0))
      ((string? $string) (string-append "string " $string))
      ((number? $number) (string-append "number " (number->string $number)))
      ((struct0? $struct0) "struct0"))
    "number 128"))

(check 
  (equal? 
    (one-of-3-switch (one-of-3 (not string) (not number) (struct0))
      ((string? $string) (string-append "string " $string))
      ((number? $number) (string-append "number " (number->string $number)))
      ((struct0? $struct0) "struct0"))
    "struct0"))
