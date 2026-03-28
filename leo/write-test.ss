(import (except (scheme) write) (check) (leo write))

(check
  (equal?
    (call-with-string-output-port
      (lambda ($port)
        (write '(hello world) $port)))
    "hello world\n"))

(check
  (equal?
    (call-with-string-output-port
      (lambda ($port)
        (write-line '(hello world) $port)))
    "hello world\n"))
