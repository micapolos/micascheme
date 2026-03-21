(library (format-datum)
  (export format->datum)
  (import (scheme))

  (define (format->datum $message . $irritants)
    (case $message
      (("variable ~:s is not bound")
        `(unbound (variable ,(car $irritants))))
      (("~s is not a number")
        `(not (number ,(car $irritants))))
      (("~s is not a string")
        `(not (string ,(car $irritants))))
      (("~s is not a character")
        `(not (character ,(car $irritants))))
      (("index ~s is out of range for list ~s")
        `(out-of-range
          (index ,(car $irritants))
          (list ,(cadr $irritants))))
      (("index ~s is not an exact nonnegative integer")
        `(not (nonnegative-integer (index ,(car $irritants)))))
      ; todo: cover all exceptions from ChezScheme
      (else
        `(message
          (apply format $message $irritants)))))

)
