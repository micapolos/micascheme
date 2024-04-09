(import (scheme) (check) (stack))

(check (equal? (stack) (list)))
(check (equal? (stack 1 2 3) (list 3 2 1)))
(check (equal? (top (stack 1 2 3)) 3))
(check (equal? (pop (stack 1 2 3)) (stack 1 2)))

(check (equal? (push (stack 1 2 3) 4) (stack 1 2 3 4)))
(check (equal? (push-list (stack 1 2 3) (list 4 5 6)) (stack 1 2 3 4 5 6)))
(check (equal? (push-all (stack 1 2 3) (stack 4 5 6)) (stack 1 2 3 4 5 6)))

(check (equal? (push... (stack 1 2 3) 4 5 6) (stack 1 2 3 4 5 6)))
