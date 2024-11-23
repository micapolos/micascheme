(import (micascheme) (micalog domain) (micalog utils) (prefix (micalog keywords) %))


; === domain+ ===

(check
  (equal?
    (syntax->datum (domain+ #'() #'()))
    '()))

(check
  (equal?
    (syntax->datum (domain+ #'() #'((clock-1 %posedge))))
    '()))

(check
  (equal?
    (syntax->datum (domain+ #'((clock-1 %posedge)) #'()))
    '()))

(check
  (equal?
    (syntax->datum (domain+ #'((clock-1 %negedge)) #'((clock-1 %posedge))))
    '()))

(check
  (equal?
    (syntax->datum (domain+ #'((clock-2 %posedge)) #'((clock-1 %posedge))))
    '()))

(check
  (equal?
    (syntax->datum (domain+ #'((clock-1 %posedge)) #'((clock-1 %posedge))))
    '((clock-1 %posedge))))

(check
  (equal?
    (syntax->datum (domain+ #'((clock-1 %posedge) (clock-2 %negedge)) #'((clock-1 %posedge))))
    '((clock-1 %posedge))))
