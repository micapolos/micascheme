(library (leo datum)
  (export ->datum)
  (import (micascheme))

  (define (->datum $atom)
    (switch $atom
      ((pair? $pair)
        (switch (car $pair)
          ((symbol? $symbol)
            (cons
              (->datum $symbol)
              (map* ->datum ->datum (cdr $pair))))
          ((else $other)
            `(the (list ,@(map* ->datum ->datum $pair))))))
      ((else $atom)
        (the->datum $atom))))

  (define (the->datum $atom)
    (switch $atom
      ((symbol? $symbol) $symbol)
      ((number? $number) $number)
      ((string? $string) $string)
      ((null? $null) '(the null))
      ((boolean? $boolean)
        `(the ,(if $boolean 'true 'false)))
      ((char? $char)
        `(the
          (char
            ,(lets
              ($string (format "~s" $char))
              ($string (substring $string 2 (string-length $string)))
              (switch (string-ref $string 0)
                ((char-numeric? $char-numeric)
                  (-
                    (char->integer $char)
                    (char->integer #\0)))
                ((else _)
                  (string->symbol $string)))))))
      ((vector? $vector)
        `(the
          (vector
            ,@(map ->datum (vector->list $vector)))))
      ((bytevector? $bytevector)
        `(the
          (bytevector
            ,@(map ->datum (bytevector->u8-list $bytevector)))))
      ((else $other)
        $other)))
)
