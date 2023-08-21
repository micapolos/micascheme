(import (micascheme) (syntax))

; tuple-syntax

(check (equal? (syntax->datum (tuple-syntax (list))) #f))
(check (equal? (syntax->datum (tuple-syntax (list #`a))) `a))
(check (equal? (syntax->datum (tuple-syntax (list #`a #`b))) `(cons a b)))
(check (equal? (syntax->datum (tuple-syntax (list #`a #`b #`c))) `(vector a b c)))

; tuple-ref-syntax

(check (equal? (syntax->datum (tuple-ref-syntax #`tuple 0 1)) `tuple))
(check (equal? (syntax->datum (tuple-ref-syntax #`tuple 0 2)) `(car tuple)))
(check (equal? (syntax->datum (tuple-ref-syntax #`tuple 1 2)) `(cdr tuple)))
(check (equal? (syntax->datum (tuple-ref-syntax #`tuple 0 3)) `(vector-ref tuple 0)))
(check (equal? (syntax->datum (tuple-ref-syntax #`tuple 1 3)) `(vector-ref tuple 1)))
(check (equal? (syntax->datum (tuple-ref-syntax #`tuple 2 3)) `(vector-ref tuple 2)))

; index-syntax

(check (equal? (syntax->datum (index-syntax 0 1)) #f))
(check (equal? (syntax->datum (index-syntax 0 2)) #t))
(check (equal? (syntax->datum (index-syntax 1 2)) #f))
(check (equal? (syntax->datum (index-syntax 0 3)) 0))
(check (equal? (syntax->datum (index-syntax 1 3)) 1))
(check (equal? (syntax->datum (index-syntax 2 3)) 2))

; index-switch-syntax

(check (equal? (syntax->datum (index-switch-syntax #`index (list #`a))) `a)) 
(check (equal? (syntax->datum (index-switch-syntax #`index (list #`a #`b))) `(if index a b)))
(check (equal? (syntax->datum (index-switch-syntax #`index (list #`a #`b #`c))) `(case index ((0) a) ((1) b) (else c))))
