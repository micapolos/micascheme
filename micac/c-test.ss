(import (micascheme) (micac c) (check) (micac scope))

(micac-externs x)

(micac-define add-all
  (lambda ($syntax)
    (syntax-case $syntax ()
      ((_ var xs ...)
        #`(begin
          #,@(map-with
            ($x (syntax->list #'(xs ...)))
            #`(set var + #,$x)))))))

(micac-define sum
  (lambda ($syntax)
    (syntax-case $syntax ()
      ((_) #'0)
      ((_ x xs ...)
        (fold-left
          (lambda ($acc $x)
            #`(+ #,$acc #,$x))
          #'x
          (syntax->list #'(xs ...)))))))

(check
  (equal?
    (micac-c
      (set x 10))
    (lines-string
      "x = 10;")))

(check
  (equal?
    (micac-c
      (add-all x 10 20 30))
    (lines-string
      "x += 10;"
      "x += 20;"
      "x += 30;")))

(check
  (equal?
    (micac-c
      (set x (sum)))
    (lines-string
      "x = 0;")))

(check
  (equal?
    (micac-c
      (set x (sum x x x)))
    (lines-string
      "x = x + x + x;")))
