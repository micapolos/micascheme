(import (micascheme) (minic stako))

; const-8
(check (equal? (stako 10 (const-8 30)) 30))

; math-8
(check (equal? (stako 10 (const-8 30) (inc-8)) 31))
(check (equal? (stako 10 (const-8 30) (dec-8)) 29))
(check (equal? (stako 10 (const-8 30) (const-8 20) (add-8)) 50))
(check (equal? (stako 10 (const-8 30) (const-8 20) (sub-8)) 10))

; block
(check (equal? (stako 10 (block (const-8 30))) 30))

; switch-8
(check (equal? (stako 10 (const-8 0) (switch-8 (const-8 10) (const-8 20) (const-8 30))) 10))
(check (equal? (stako 10 (const-8 1) (switch-8 (const-8 10) (const-8 20) (const-8 30))) 20))
(check (equal? (stako 10 (const-8 2) (switch-8 (const-8 10) (const-8 20) (const-8 30))) 30))
(check (equal? (stako 10 (const-8 3) (switch-8 (const-8 10) (const-8 20) (const-8 30))) 30))
