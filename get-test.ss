(import
  (scheme)
  (check)
  (lets)
  (eof)
  (get)
  (procedure))

(lets
  ($get! (substring->get-char/eof! "abcde" 1 4))
  (run
    (check (equal? ($get!) #\b))
    (check (equal? ($get!) #\c))
    (check (equal? ($get!) #\d))
    (check (equal? ($get!) eof))))

(lets
  ($get! (string->get-char/eof! "abc"))
  (run
    (check (equal? ($get!) #\a))
    (check (equal? ($get!) #\b))
    (check (equal? ($get!) #\c))
    (check (equal? ($get!) eof))))

(lets
  ($get! (textual-port->get-char/eof! (open-input-string "abc")))
  (run
    (check (equal? ($get!) #\a))
    (check (equal? ($get!) #\b))
    (check (equal? ($get!) #\c))
    (check (equal? ($get!) eof))))
