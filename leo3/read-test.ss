(import
  (scheme)
  (check)
  (lets)
  (procedure)
  (eof)
  (prefix (leo3 read) leo-))

(check
  (equal?
    (map annotation-stripped (leo-read-file "leo3/test.leo"))
    '(10 "foo" foo foo-bar (foo bar) (foo-bar foo-bar) (foo-bar 10 20 30))))

(check
  (equal?
    (map annotation-stripped (leo-read-file "leo3/test-script.leo"))
    '(
      (import
        (from scheme)
        (from leo3 test-library))
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

