(import
  (scheme)
  (check)
  (union))

(union (literal boolean number string))

(define (literal->datum $literal)
  (literal-switch $literal
    ((boolean? $boolean) `(boolean ,$boolean))
    ((number? $number) `(number ,$number))
    ((string? $string) `(string ,$string))))

(define (obj->datum $literal)
  (literal-switch $literal
    ((boolean? $boolean) `(boolean ,$boolean))
    ((number? $number) `(number ,$number))
    ((string? $string) `(string ,$string))
    ((else $other) `(other ,$other))))

(check (literal? #f))
(check (literal? 123))
(check (literal? "foo"))
(check (not (literal? #\a)))

(check (equal? (literal->datum #f) '(boolean #f)))
(check (equal? (literal->datum 123) '(number 123)))
(check (equal? (literal->datum "foo") '(string "foo")))
(check (raises (literal->datum #\a)))

(check (equal? (obj->datum #f) '(boolean #f)))
(check (equal? (obj->datum 123) '(number 123)))
(check (equal? (obj->datum "foo") '(string "foo")))
(check (equal? (obj->datum #\a) '(other #\a)))
