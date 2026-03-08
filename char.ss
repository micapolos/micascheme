(library (char)
  (export
    char->ascii
    char-newline?
    char-space?)
  (import (scheme))

  (define (char->ascii $char)
    (bitwise-and #xff (char->integer $char)))

  (define (char-newline? $char) (char=? $char #\newline))
  (define (char-space? $char) (char=? $char #\space))
)
