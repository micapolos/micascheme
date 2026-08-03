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

  (define datum=? (unchecked (pi (datum datum) boolean) %equal?))
  (define boolean->datum (unchecked (pi (boolean) datum) %identity))
  (define number->datum (unchecked (pi (number) datum) %identity))
  (define char->datum (unchecked (pi (char) datum) %identity))
  (define string->datum (unchecked (pi (string) datum) %identity))
  (define cons (unchecked (pi (datum datum) datum) %cons))
  (define datum-append (unchecked (pi (datum ...) datum) %list))
  (define list->datum (unchecked (pi ((list datum)) datum) %identity))
)
