(import (micascheme) (micac syntax-expr))

(define-rule-syntax (check-fold-constants expr expected)
  (check
    (equal?
      (syntax->datum (fold-constants identity #`expr))
      (syntax->datum #`expected))))

(check-fold-constants (+) 0)
(check-fold-constants (+ 1 2 3) 6)
(check-fold-constants (+ 1 a 2 b 3) (+ 6 a b))

(check-fold-constants (and) -1)
(check-fold-constants (and #xff #x0f #xcc) #x0c)
(check-fold-constants (and #xff a #x0f b #xcc) (and #x0c a b))

(check-fold-constants (or) 0)
(check-fold-constants (or #x00 #x0f #xcc) #xcf)
(check-fold-constants (or #x00 a #x0f b #xcc) (or #xcf a b))

(check-fold-constants (xor) 0)
(check-fold-constants (xor #xff #x0f #xcc) #x3c)
(check-fold-constants (xor #xff a #x0f b #xcc) (xor #x3c a b))

