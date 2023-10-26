(import (micascheme) (tico expression))

(check
  (equal?
    (with-generate-temporary-seed $tmp
      (lambda-expression 2
        (lambda ($params)
          `(string-append ,@$params))))
    `(lambda ($tmp-0 $tmp-1)
      (string-append $tmp-0 $tmp-1))))

(check
  (equal?
    (apply-expression `fn (list `v1 `v2))
    `(fn v1 v2)))

(check
  (equal?
    (with-generate-temporary-seed $tmp
      (let-expression
        (list `v1 `v2)
        (lambda ($params)
          `(string-append ,@$params))))
    `(let (($tmp-0 v1) ($tmp-1 v2))
      (string-append $tmp-0 $tmp-1))))

(check (equal? (tuple-expression (list)) #f))
(check (equal? (tuple-expression (list `v1)) `v1))
(check (equal? (tuple-expression (list `v1 `v2)) `(cons v1 v2)))
(check (equal? (tuple-expression (list `v1 `v2 `v3)) `(vector v1 v2 v3)))

(check (equal? (tuple-ref-expression 1 `t 0) `t))
(check (equal? (tuple-ref-expression 2 `t 0) `(car t)))
(check (equal? (tuple-ref-expression 2 `t 1) `(cdr t)))
(check (equal? (tuple-ref-expression 3 `t 0) `(vector-ref t 0)))
(check (equal? (tuple-ref-expression 3 `t 1) `(vector-ref t 1)))
(check (equal? (tuple-ref-expression 3 `t 2) `(vector-ref t 2)))

(check (equal? (selector-expression 1 0) #f))
(check (equal? (selector-expression 2 0) #t))
(check (equal? (selector-expression 2 1) #f))
(check (equal? (selector-expression 3 0) 0))
(check (equal? (selector-expression 3 1) 1))
(check (equal? (selector-expression 3 2) 2))

(check (equal? (switch-expression `cond (list `c0)) `c0))
(check (equal? (switch-expression `cond (list `c0 `c1)) `(if cond c0 c1)))
(check (equal? (switch-expression `cond (list `c0 `c1 `c2)) `(index-switch cond c0 c1 c2)))
