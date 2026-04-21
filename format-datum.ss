(library (format-datum)
  (export format->datum)
  (import (scheme))

  (define (format->datum $message . $irritants)
    (case $message
      ; libraries
      (("library ~s not found")
        `(missing (library ,(car $irritants))))
      (("loading ~a did not define library ~s")
        `(sequence
          (loading ,(car $irritants))
          (missing (library ,(cadr $irritants)))))

      ; variables
      (("variable ~:s is not bound")
        `(unbound (variable ,(car $irritants))))

      ; types
      (("attempt to apply non-procedure ~s")
        `(attempt (to (apply (non (procedure ,(car $irritants)))))))
      (("possible incorrect argument count in call ~a")
        `(possible (incorrect (argument (count (in (call
          ,(read (open-input-string (car $irritants))))))))))
      (("incorrect number of arguments ~s to ~s")
        `(incorrect
          (number (of (arguments ,(car $irritants))))
          (to ,(cadr $irritants))))

      (("~s is not a pair")
        `(not (pair? ,(car $irritants))))
      (("~s is not a fixnum")
        `(not (fixnum? ,(car $irritants))))
      (("~s is not a flonum")
        `(not (flonum? ,(car $irritants))))
      (("~s is not an integer")
        `(not (integer? ,(car $irritants))))
      (("~s is not a number")
        `(not (number? ,(car $irritants))))
      (("~s is not a character")
        `(not (char? ,(car $irritants))))
      (("~s is not a string")
        `(not (string? ,(car $irritants))))
      (("~s is not a proper list")
        `(not (list? ,(car $irritants))))
      (("~s is not a vector")
        `(not (vector? ,(car $irritants))))
      (("~s is not a bytevector")
        `(not (bytevector? ,(car $irritants))))
      (("~s is not a box")
        `(not (box? ,(car $irritants))))
      (("invalid value ~s")
        `(invalid (value ,(car $irritants))))

      (("undefined for ~s")
        `(undefined (for ,(car $irritants))))

      (("lists ~s and ~s differ in length")
        `(different (length (of ,(car $irritants) ,(cadr $irritants)))))

      (("~s is not a valid index for ~s")
        `(invalid
          (index ,(car $irritants))
          (for ,(cadr $irritants))))
      (("index ~s is out of range for list ~s")
        `(invalid
          (index ,(car $irritants))
          (for ,(cadr $irritants))))
      (("index ~s is not an exact nonnegative integer")
        `(not (nonnegative-integer (index ,(car $irritants)))))
      ; todo: cover all exceptions from ChezScheme
      ; or implement smart translation from string to datum
      (else
        `(message
          ,(apply format $message $irritants)))))

)
