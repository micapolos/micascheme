(import
  (scheme)
  (check)
  (lets)
  (procedure)
  (eof)
  (prefix (leo3 read) leo-))

(check (eof? (leo-read (open-input-string ""))))

(check (equal? (leo-read (open-input-string "is foo bar\n")) 'foo-bar?))
