(library (ansi-string)
  (export
    ansi-string
    ansi-black-string
    ansi-purple-string
    ansi-cyan-string
    ansi-bright-red-string)
  (import (scheme))

  (define (ansi-string $number)
    (string-append
      "\x1b;["
      (number->string $number)
      "m"))

  (define ansi-black-string (ansi-string 0))
  (define ansi-purple-string (ansi-string 35))
  (define ansi-cyan-string (ansi-string 36))
  (define ansi-bright-red-string (ansi-string 91))
)
