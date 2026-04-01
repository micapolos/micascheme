(library (leo datum)
  (export
    null-datum
    boolean->datum
    char->leo-datum
    bytevector->datum
    vector->datum
    ->datum)
  (import
    (scheme)
    (char)
    (switch)
    (list))

  (define null-datum 'null)

  (define (boolean->datum $boolean)
    (if $boolean 'true 'false))

  (define (char->leo-datum $char)
    `(char ,(char->datum $char)))

  (define (bytevector->datum $bytevector)
    `(bytevector ,@(bytevector->u8-list $bytevector)))

  (define (vector->datum $vector)
    `(vector ,@(map ->datum (vector->list $vector))))

  (define (->datum $atom)
    (switch $atom
      ((pair? $pair)
        (switch (car $pair)
          ((symbol? $symbol)
            (cons
              (->datum $symbol)
              (map* ->datum ->datum (cdr $pair))))
          ((else $other)
            `(list ,@(map* ->datum ->datum $pair)))))
      ((else $atom)
        (atom->datum $atom))))

  (define (atom->datum $atom)
    (switch $atom
      ((symbol? $symbol) $symbol)
      ((number? $number) $number)
      ((string? $string) $string)
      ((null? $null) null-datum)
      ((boolean? $boolean) (boolean->datum $boolean))
      ((char? $char) (char->leo-datum $char))
      ((vector? $vector) (vector->datum $vector))
      ((bytevector? $bytevector) (bytevector->datum $bytevector))
      ((else $other) $other)))
)
