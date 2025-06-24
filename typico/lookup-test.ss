(import (micascheme) (typico lookup) (typico expand) (typico typed))

(define test-lookup (lookup+let (empty-lookup)))

(define-rule-syntax (check-typed in out)
  (check
    (equal?
      (typed->datum (expand-typed test-lookup (syntax->datum/annotation #'in)))
      'out)))

(check
  (equal?
    (typed->datum (((lookup+let (empty-lookup)) 'let) test-lookup #'(let () 123)))
    '(typed integer (let () 123))))

(check
  (equal?
    (typed->datum (((lookup+let (empty-lookup)) 'let) test-lookup #'(let ((x 10)) x)))
    '(typed integer (let ((x 10)) x))))

(check
  (equal?
    (typed->datum (((lookup+let (empty-lookup)) 'let) test-lookup #'(let ((x 10) (y 20)) z)))
    '(typed integer (let ((x 10)) x))))
