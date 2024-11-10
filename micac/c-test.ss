(import (micascheme) (micac c) (check))

(define-micac add-all
  (lambda ($syntax)
    (syntax-case $syntax ()
      ((_ var xs ...)
        #`(begin
          #,@(map-with
            ($x (syntax->list #'(xs ...)))
            #`(set+ var #,$x)))))))

(define-micac sum
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
      (var int x)
      (set x 10))
    (lines-string
      "int x;"
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
      (set x (sum 10 20 30)))
    (lines-string
      "x = 10 + 20 + 30;")))
