(library (tt string)
  (export
    string
    string-length
    string=?
    string+
    string-append
    join-string
    list->string
    string->list
    number->string)
  (import
    (tt lang)
    (tt number)
    (tt list)
    (prefix (scheme) %)
    (prefix (procedure) %))

  (define string (unchecked (pi (char ...) string) %string))
  (define string=? (unchecked (pi (string string) boolean) %string=?))
  (define string-length (unchecked (pi (string) number) %string-length))
  (define string+ (unchecked (pi (string string) string) %string-append))
  (define list->string (unchecked (pi ((list char)) string) %list->string))
  (define string->list (unchecked (pi (string) (list char)) %string->list))
  (define join-string (unchecked (pi ((list string)) string) (%partial %apply %string-append)))
  (define string-append (unchecked (pi (string ...) string) %string-append))
  (define number->string (unchecked (pi (number) string) %number->string))
)
