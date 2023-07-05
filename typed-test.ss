(import (micascheme) (typed) (type))

(check 
  (obj=?
    (parse
      (list 
        (arrow `(length ,(any-string)) (any-number))
        (arrow `(string ,(any-number)) (any-string))
        (arrow `(append ,(any-string) ,(any-string)) (any-string)))
      #`(append (string (length "foo")) " apples"))
    (typed 
      `(v2 (v1 (v0 "foo")) " apples")
      (any-string))))

(check
  (obj=?
    (evaluate
      (list 
        (cons `string-length (arrow `(length ,(any-string)) (any-number)))
        (cons `number->string (arrow `(string ,(any-number)) (any-string)))
        (cons `string-append (arrow `(append ,(any-string) ,(any-string)) (any-string))))
      #`(append (string (length "foo")) " apples"))
    (typed "3 apples" (any-string))))
