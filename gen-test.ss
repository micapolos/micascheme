(import (micascheme) (gen))

(check 
  (equal? 
    (gen-value 
      (gen "foo") 
      `any-state) 
    "foo"))

(check 
  (equal? 
    (gen-value 
      (gen-bind! 
        ($foo (gen "foo"))
        (gen (string-append $foo "!"))) 
      `any-state) 
    "foo!"))

(check
  (equal? 
    (gen-value 
      (gen-map! 
        ($foo (gen "foo")) 
        (string-append $foo "!")) 
      `any-state) 
    "foo!"))

(check
  (equal? 
    (gen-value state-gen `state)
    `state))
