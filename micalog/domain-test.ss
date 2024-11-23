(import (micascheme) (micalog domain) (prefix (micalog keywords) %))

; === edges+edge ===

(check
  (equal?
    (syntax->datum (edges+edge #'(foo bar) #'%posedge))
    '(#t bar)))

(check
  (equal?
    (syntax->datum (edges+edge #'(foo bar) #'%negedge))
    '(foo #t)))

; === edges ===

(check
  (equal?
    (syntax->datum (edges+ #'(#f #t) #'(#t #f)))
    '(#t #t)))

(check
  (equal?
    (syntax->datum (edges+ #'(#f #f) #'(#t #f)))
    '(#t #f)))

; === domain+ ===

(check
  (equal?
    (syntax->datum (domain+ #'() #'()))
    '()))

(check
  (equal?
    (syntax->datum (domain+ #'() #'((clock-1 #t #f))))
    '((clock-1 #t #f))))

(check
  (equal?
    (syntax->datum (domain+ #'((clock-1 #t #f)) #'()))
    '((clock-1 #t #f))))

(check
  (equal?
    (syntax->datum (domain+ #'((clock-1 #t #f)) #'((clock-1 #t #t))))
    '((clock-1 #t #t))))

(check
  (equal?
    (syntax->datum (domain+ #'((clock-1 #t #f) (clock-2 #t #f)) #'((clock-1 #t #t))))
    '((clock-1 #t #t) (clock-2 #t #f))))

(check
  (raises
    (syntax->datum (domain+ #'((clock-1 #t #f)) #'((clock-2 #t #t))))))
