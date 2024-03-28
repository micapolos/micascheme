(import (check) (labs js))

(define-syntax (js $syntax)
  (syntax-case $syntax ()
    ((_ $expr)
      (datum->syntax #'js
        (parse-js
          (datum->syntax #'js
            (expand
              (syntax->datum #'$expr))))))))

(check
  (equal?
    (js (let ((x 1) (y 2)) (+ x y)))
    "{ let x = 1; let y = 2; x + y }"))
