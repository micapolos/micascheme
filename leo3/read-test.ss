(import
  (scheme)
  (check)
  (lets)
  (procedure)
  (eof)
  (prefix (leo3 read) leo-))

(check (eof? (leo-read (open-input-string ""))))

(check (equal? (leo-read (open-input-string "is foo bar\n")) 'foo-bar?))

(lets
  ($port (open-input-string "foo\n10\n\"bar\"\n"))
  (run
    (check (equal? (leo-read $port) 'foo))
    (check (equal? (leo-read $port) 10))
    (check (equal? (leo-read $port) "bar"))
    (check (eof? (leo-read $port)))))

(check
  (equal?
    (map annotation-stripped (leo-read-file "leo3/test.leo"))
    '(10 "foo" foo foo-bar (foo bar) (foo-bar foo-bar) (foo-bar 10 20 30))))
