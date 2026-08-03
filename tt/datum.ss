(library (tt datum)
  (export
    datum=?
    boolean->datum
    number->datum
    char->datum
    string->datum
    cons
    datum-append
    list->datum)
  (import
    (tt lang)
    (tt list)
    (prefix (scheme) %)
    (prefix (procedure) %))

  (define (datum=? (d1 datum) (d2 datum))
    (unchecked boolean (%equal? d1 d2)))

  (define (boolean->datum (b boolean))
    (unchecked datum b))

  (define (number->datum (n number))
    (unchecked datum n))

  (define (char->datum (ch char))
    (unchecked datum ch))

  (define (string->datum (s string))
    (unchecked datum s))

  (define (cons (car datum) (cdr datum))
    (unchecked datum (%cons car cdr)))

  (define datum-append
    (unchecked
      (pi (datum ...) datum)
      %list))

  (define list->datum
    (unchecked
      (pi ((list datum)) datum)
      %identity))
)
