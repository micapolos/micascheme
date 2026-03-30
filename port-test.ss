(import (scheme) (check) (port) (string))

(check
  (equal?
    (call-with-string-output-port
      (lambda ($port)
        (let (($prefixed-port (make-prefixed-textual-output-port $port "> ")))
          (put-string $prefixed-port
            (lines-string
              "line 1"
              "line 2"
              "line 3"))
          (flush-output-port $prefixed-port))))
    (lines-string
      "> line 1"
      "> line 2"
      "> line 3")))
