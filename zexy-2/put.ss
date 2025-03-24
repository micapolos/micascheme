(library (zexy-2 put)
  (export put-u16)
  (import (except (micascheme) put-u16) (zexy math))

  (define (put-u16 $port $word)
    (run
      (put-u8 $port (lsb $word))
      (put-u8 $port (msb $word))))
)
