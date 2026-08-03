(library (tt char)
  (export char=? char->number)
  (import
    (tt lang)
    (tt boolean)
    (prefix (scheme) %))

  (define char=? (unchecked (pi (char char) boolean) %char=?))
  (define char->number (unchecked (pi (char) number) %char->integer))
)
