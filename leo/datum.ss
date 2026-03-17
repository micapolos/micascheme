(library (leo datum)
  (export atom->datum)
  (import (micascheme))

  (define (atom->datum $atom)
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
          ,@(map atom->datum (vector->list $vector))))
      ((bytevector? $bytevector)
        `(bytevector
          ,@(map atom->datum (bytevector->u8-list $bytevector))))
      ((pair? $pair)
        `(
          ,(atom->datum (car $pair))
          ,(atom->datum (cdr $pair))))
      ((else $other)
        $other)))
)
