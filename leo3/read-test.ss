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

(check
  (equal?
    (map annotation-stripped (leo-read-file "leo3/test-script.leo"))
    '(
      (import (scheme))
      (define hello "Hello")
      (define world "world")
      (define (comma-separated first second)
        (string-append first ", " second))
      (define (exclamated string)
        (string-append string "!"))
      (define (display-line string)
        (display string)
        (newline))
      (display-line (exclamated (comma-separated hello world))))))

