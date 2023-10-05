(import (micascheme) (z80-parser))

(check
  (equal?
    (syntax->bytevector
      #`(
        (call $sub-routine)
        $sub-routine
        (ret)))
    #vu8(
      #xCD #x03 #x00
      #xC9)))
