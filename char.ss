(library (char)
  (export
    char->ascii
    char-newline?
    char-space?
    char-comma?
    char-dot?
    char-colon?)
  (import (scheme))

  (define (char->ascii $char)
    (bitwise-and #xff (char->integer $char)))

  (define (char-newline? $char) (char=? $char #\newline))
  (define (char-space? $char) (char=? $char #\space))
  (define (char-comma? $char) (char=? $char #\,))
  (define (char-dot? $char) (char=? $char #\.))
  (define (char-colon? $char) (char=? $char #\:))
)
