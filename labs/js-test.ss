(import (check) (labs js))

(check
  (equal?
    (parse-js
      #`(let ((x 1) (y 2)) (+ x y)))
    "{ let x = 1; let y = 2; x + y }"))
