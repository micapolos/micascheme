(library (zx-next emu runtime)
  (export
    u8-ref u16-ref u32-ref
    u8-set! u16-set! u32-set!
    u16-lo-ref u16-hi-ref)
  (import (micascheme))

  (define-case-syntaxes
    ((u8-ref address)
      (with-implicit (u8-ref mem)
        #'(bytevector-u8-ref mem address)))

    ((u16-ref address)
      (with-implicit (u16-ref mem)
        #'(bytevector-u16-native-ref mem address)))
    ((u16-ref address endianness)
      (with-implicit (u16-ref mem)
        #'(bytevector-u16-ref mem address endianness)))

    ((u32-ref address)
      (with-implicit (u32-ref mem)
        #'(bytevector-u32-native-ref mem address)))
    ((u32-ref address endianness)
      (with-implicit (u32-ref mem)
        #'(bytevector-u32-ref mem address endianness)))

    ((u8-set! address u8)
      (with-implicit (u8-set! mem)
        #'(bytevector-u8-set! mem address u8)))

    ((u16-set! address u16)
      (with-implicit (u16-set! mem)
        #'(bytevector-u16-native-set! mem address u16)))
    ((u16-set! address u16 endianness)
      (with-implicit (u16-set! mem)
        #'(bytevector-u16-set! mem address u16 endianness)))

    ((u32-set! address u32)
      (with-implicit (u32-set! mem)
        #'(bytevector-u32-native-set! mem address u32)))
    ((u32-set-le! address u32 endianness)
      (with-implicit (u32-set! mem)
        #'(bytevector-u32-set! mem address u32 endianness)))

    ((u16-hi-ref address)
      (with-implicit (u16-hi-ref mem)
        #'(bytevector-u8-ref mem (+ address (if (symbol=? (native-endianness) (endianness little)) 1 0)))))
    ((u16-lo-ref address)
      (with-implicit (u16-lo-ref mem)
        #'(bytevector-u8-ref mem (+ address (if (symbol=? (native-endianness) (endianness little)) 0 1)))))
  )
)
