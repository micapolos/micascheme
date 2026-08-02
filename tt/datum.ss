(library (tt datum)
  (export
    boolean->datum
    number->datum
    char->datum
    string->datum
    datum=?
    datum->datum
    cons)
  (import
    (tt lang)
    (prefix (scheme) %))

  (define (boolean->datum (b boolean))
    (unchecked datum n))

  (define (number->datum (n number))
    (unchecked datum n))

  (define (char->datum (ch char))
    (unchecked datum ch))

  (define (string->datum (s string))
    (unchecked datum s))

  (define (datum=? (d1 datum) (d2 datum))
    (unchecked boolean (%equal? d1 d2)))

  (define (datum->datum (d datum)) d)

  (define (cons (car datum) (cdr datum))
    (unchecked datum (%cons car cdr)))
)
