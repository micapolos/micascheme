(library (tt string)
  (export string-length string=? string-append)
  (import
    (tt lang)
    (prefix (scheme) %))

  (define string=? (unchecked (pi (string string) boolean) %string=?))
  (define string-length (unchecked (pi (string) number) %string-length))
  (define string-append (unchecked (pi (string string) string) %string-append))
)
