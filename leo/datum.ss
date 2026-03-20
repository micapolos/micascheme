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
            `(: ,@(map* ->datum ->datum $pair)))))
      ((else $atom)
        (atom->datum $atom))))

  (define (atom->datum $atom)
    (switch $atom
      ((symbol? $symbol) $symbol)
      ((number? $number) $number)
      ((string? $string) $string)
      ((null? $null) (string->symbol "#null"))
      ((boolean? $boolean)
        (string->symbol (if $boolean "#true" "#false")))
      ((char? $char)
        `(,(string->symbol "#char")
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
        `(
          ,(string->symbol "#vector")
          ,@(map ->datum (vector->list $vector))))
      ((bytevector? $bytevector)
        `(
          ,(string->symbol "#bytevector")
          ,@(map ->datum (bytevector->u8-list $bytevector))))
      ((else $other)
        $other)))
)
