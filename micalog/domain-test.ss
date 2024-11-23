(import (micascheme) (micalog domain) (prefix (micalog keywords) %))

; === edge+ ===

(check (equal? (syntax->datum (edge+ #'%posedge #'%posedge)) '%posedge))
(check (equal? (syntax->datum (edge+ #'%negedge #'%negedge)) '%negedge))
(check (raises (edge+ #'%posedge #'%negedge)))
(check (raises (edge+ #'%negedge #'%posedge)))

; === domain+ ===

(check
  (equal?
    (syntax->datum (domain+ #'() #'()))
    '()))

(check
  (equal?
    (syntax->datum (domain+ #'() #'((clock-1 %posedge))))
    '((clock-1 %posedge))))

(check
  (equal?
    (syntax->datum (domain+ #'((clock-1 %posedge)) #'()))
    '((clock-1 %posedge))))

(check
  (equal?
    (syntax->datum (domain+ #'((clock-1 %posedge)) #'((clock-1 %posedge))))
    '((clock-1 %posedge))))

(check
  (raises
    (domain+ #'((clock-1 %posedge)) #'((clock-1 %negedge)))))

(check
  (raises
    (domain+ #'((clock-1 %posedge)) #'((clock-2 %posedge)))))

(check
  (equal?
    (syntax->datum (domain+ #'((clock-1 %posedge) (clock-2 %negedge)) #'((clock-1 %posedge))))
    '((clock-1 %posedge) (clock-2 %negedge))))

; === env+ ===

(check
  (equal?
    (syntax->datum
      (env+id-domain
        #`(
          (reg-1 (clock %posedge))
          (reg-2 (clock %negedge)))
        #'reg-3
        #'((clock %posedge) (clock-2 %negedge))))
    '(
      (reg-1 (clock %posedge))
      (reg-2 (clock %negedge))
      (reg-3 (clock %posedge) (clock-2 %negedge)))))
