(import (micascheme) (micalog domain) (prefix (micalog keywords) %))

(check
  (equal?
    (syntax->datum (edges+edge #'(foo bar) #'%posedge))
    '(#t bar)))

(check
  (equal?
    (syntax->datum (edges+edge #'(foo bar) #'%negedge))
    '(foo #t)))
