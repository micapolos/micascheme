(library (leo datum)
  (export ->datum)
  (import (micascheme))

  (define (->datum $atom)
    (switch $atom
      ((boolean? $boolean)
        (if $boolean 'true 'false))
      ((char? $char)
        `(char
          ,(lets
            ($string (format "~s" $char))
            ($string (substring $string 2 (string-length $string)))
            (switch (string-ref $string 0)
              ((char-numeric? $char-numeric)
                (-
                  (char->integer $char)
                  (char->integer #\0)))
              ((else _)
                (string->symbol $string))))))
      ((vector? $vector)
        `(vector
          ,@(map ->datum (vector->list $vector))))
      ((bytevector? $bytevector)
        `(bytevector
          ,@(map ->datum (bytevector->u8-list $bytevector))))
      ((pair? $pair)
        (cons
          (->datum (car $pair))
          (->datum (cdr $pair))))
      ((else $other)
        $other)))
)
