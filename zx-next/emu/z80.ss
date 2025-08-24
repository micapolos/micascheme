(library (zx-next emu z80)
  (export
    make-z80
    z80-af
    z80-bc
    z80-de
    z80-hl
    z80-af2
    z80-bc2
    z80-de2
    z80-hl2
    z80-ix
    z80-iy
    z80-pc
    z80-sp

    z80-a
    z80-f
    z80-b
    z80-c
    z80-d
    z80-e
    z80-h
    z80-l

    set-z80-af!
    set-z80-bc!
    set-z80-de!
    set-z80-hl!
    set-z80-ix!
    set-z80-iy!
    set-z80-pc!
    set-z80-sp!

    set-z80-a!
    set-z80-f!
    set-z80-b!
    set-z80-c!
    set-z80-d!
    set-z80-e!
    set-z80-h!
    set-z80-l!)

  (import (micascheme))

  (define-case-syntaxes
    (hi-offset (if (symbol=? (native-endianness) (endianness little)) #'1 #'0))
    (lo-offset (if (symbol=? (native-endianness) (endianness little)) #'0 #'1)))

  (define-case-syntaxes
    ((define-r id offset)
      #`(define-rules-syntaxes
        ((#,(identifier-append #'id #'z80- #'id) z80)
          (bytevector-u8-ref z80 offset))
        ((#,(identifier-append #'id #'set-z80- #'id #'!) z80 u8)
          (bytevector-u8-set! z80 u8 offset))))
    ((define-rr id offset)
      #`(define-rules-syntaxes
        ((#,(identifier-append #'id #'z80- #'id) z80)
          (bytevector-u16-native-ref z80 offset))
        ((#,(identifier-append #'id #'set-z80- #'id #'!) z80 u16)
          (bytevector-u16-native-set! z80 u16 offset))))
    ((define-rr+alt id offset)
      #`(begin
        (define-rr id offset)
        (define-rr #,(identifier-append #'id #'id (datum->syntax #'id (string->symbol "2"))) #,(+ (datum offset) 8)))))

  (define-rr+alt af #x00)
  (define-rr+alt bc #x02)
  (define-rr+alt de #x04)
  (define-rr+alt hl #x06)
  (define-rr ix #x10)
  (define-rr iy #x12)
  (define-rr pc #x14)
  (define-rr sp #x16)

  (define-rules-syntaxes
    ((make-z80)                   (make-bytevector z80-size 0))

    (af-offset #x00)
    (hl-offset #x02)
    (bc-offset #x04)
    (de-offset #x06)

    (af2-offset #x08)
    (hl2-offset #x0a)
    (bc2-offset #x0c)
    (de2-offset #x0e)

    (ix-offset #x10)
    (iy-offset #x12)
    (pc-offset #x14)
    (sp-offset #x16)

    (z80-size #x18)

    ((z80-16 z80 offset)          (bytevector-u16-native-ref z80 offset))
    ((set-z80-16! z80 offset u16) (bytevector-u16-native-set z80 offset u16))

    ((z80-hi z80 offset)          (bytevector-u8-ref z80 (+ offset hi-offset)))
    ((z80-lo z80 offset)          (bytevector-u8-ref z80 (+ offset lo-offset)))

    ((set-z80-hi! z80 offset u8) (bytevector-u8-set! z80 (+ offset hi-offset) u8))
    ((set-z80-lo! z80 offset u8) (bytevector-u8-set! z80 (+ offset lo-offset) u8))

    ((z80-a z80)   (z80-hi z80 af-offset))
    ((z80-f z80)   (z80-lo z80 af-offset))
    ((z80-b z80)   (z80-hi z80 bc-offset))
    ((z80-c z80)   (z80-lo z80 bc-offset))
    ((z80-d z80)   (z80-hi z80 de-offset))
    ((z80-e z80)   (z80-hi z80 de-offset))
    ((z80-h z80)   (z80-lo z80 hl-offset))
    ((z80-l z80)   (z80-hi z80 hl-offset))

    ((set-z80-a! z80 u8)  (set-z80-hi! z80 af-offset u8))
    ((set-z80-f! z80 u8)  (set-z80-lo! z80 af-offset u8))
    ((set-z80-b! z80 u8)  (set-z80-hi! z80 bc-offset u8))
    ((set-z80-c! z80 u8)  (set-z80-lo! z80 bc-offset u8))
    ((set-z80-d! z80 u8)  (set-z80-hi! z80 de-offset u8))
    ((set-z80-e! z80 u8)  (set-z80-hi! z80 de-offset u8))
    ((set-z80-h! z80 u8)  (set-z80-lo! z80 hl-offset u8))
    ((set-z80-l! z80 u8)  (set-z80-hi! z80 hl-offset u8)))
)
