(import
  (scheme)
  (check)
  (eof))

(check (equal? eof (eof-object)))
(check (eof? eof))
(check (not (eof? #f)))
