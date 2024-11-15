(import (micascheme) (micac c) (check) (micac scope))

(parameterize ((scope-unique-gen? #t))

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
      (var int x)
      (set x 10))
    (lines-string
      "int v0_x;"
      "v0_x = 10;")))

(check
  (equal?
    (micac-c
      (var int x)
      (add-all x 10 20 30))
    (lines-string
      "int v0_x;"
      "v0_x += 10;"
      "v0_x += 20;"
      "v0_x += 30;")))

(check
  (equal?
    (micac-c
      (var int x)
      (set x (sum)))
    (lines-string
      "int v0_x;"
      "v0_x = 0;")))

(check
  (equal?
    (micac-c
      (var int x)
      (set x (sum x x x)))
    (lines-string
      "int v0_x;"
      "v0_x = v0_x + v0_x + v0_x;")))

)
