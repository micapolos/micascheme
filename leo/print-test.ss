(import (scheme) (check) (leo print))

(check
  (equal?
    (call-with-string-output-port
      (lambda ($port)
        (print '(hello world) $port)))
    "hello world\n"))

(check
  (equal?
    (call-with-string-output-port
      (lambda ($port)
        (print-line '(hello world) $port)))
    "hello world\n"))

(check
  (equal?
    (call-with-string-output-port
      (lambda ($port)
        (print-line '() $port)))
    "null\n"))
