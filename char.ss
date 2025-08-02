(library (char)
  (export char->ascii)
  (import (scheme))

  (define (char->ascii $char)
    (bitwise-and #xff (char->integer $char)))
)
