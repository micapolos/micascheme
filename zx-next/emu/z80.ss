(library (zx-next emu z80)
  (export
    make-z80
    z80-af
    z80-bc
    z80-de
    z80-hl
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

  (define-syntax (hi-offset $syntax)
    (syntax-case $syntax (hi-offset)
      (hi-offset (if (symbol=? (native-endianness) (endianness little)) #'1 #'0))))

  (define-syntax (lo-offset $syntax)
    (syntax-case $syntax (lo-offset)
      (lo-offset (if (symbol=? (native-endianness) (endianness little)) #'0 #'1))))

  (define-rules-syntaxes
    ((make-z80)                   (make-bytevector #x20 0))

    ((z80-16 z80 offset)          (bytevector-u16-native-ref z80 offset))
    ((set-z80-16! z80 offset u16) (bytevector-u16-native-set z80 offset u16))

    ((z80-hi z80 offset)          (bytevector-u8-ref z80 (+ offset hi-offset)))
    ((z80-lo z80 offset)          (bytevector-u8-ref z80 (+ offset lo-offset)))

    ((set-z80-hi! z80 offset u8) (bytevector-u8-set! z80 (+ offset hi-offset) u8))
    ((set-z80-lo! z80 offset u8) (bytevector-u8-set! z80 (+ offset lo-offset) u8))

    ((z80-af z80)  (z80-16 z80 #x00))
    ((z80-bc z80)  (z80-16 z80 #x02))
    ((z80-de z80)  (z80-16 z80 #x04))
    ((z80-hl z80)  (z80-16 z80 #x06))

    ((z80-ix z80)  (z80-16 z80 #x10))
    ((z80-iy z80)  (z80-16 z80 #x12))
    ((z80-pc z80)  (z80-16 z80 #x14))
    ((z80-sp z80)  (z80-16 z80 #x16))

    ((set-z80-af! z80 u16)  (set-z80-16! z80 #x00 u16))
    ((set-z80-bc! z80 u16)  (set-z80-16! z80 #x02 u16))
    ((set-z80-de! z80 u16)  (set-z80-16! z80 #x04 u16))
    ((set-z80-hl! z80 u16)  (set-z80-16! z80 #x06 u16))

    ((set-z80-ix! z80 u16)  (set-z80-16! z80 #x10 u16))
    ((set-z80-iy! z80 u16)  (set-z80-16! z80 #x12 u16))
    ((set-z80-pc! z80 u16)  (set-z80-16! z80 #x14 u16))
    ((set-z80-sp! z80 u16)  (set-z80-16! z80 #x16 u16))

    ((z80-a z80)   (z80-hi z80 #x00))
    ((z80-f z80)   (z80-lo z80 #x00))
    ((z80-b z80)   (z80-hi z80 #x02))
    ((z80-c z80)   (z80-lo z80 #x02))
    ((z80-d z80)   (z80-hi z80 #x04))
    ((z80-e z80)   (z80-hi z80 #x04))
    ((z80-h z80)   (z80-lo z80 #x06))
    ((z80-l z80)   (z80-hi z80 #x06))

    ((set-z80-a! z80 u8)  (set-z80-hi! z80 #x00 u8))
    ((set-z80-f! z80 u8)  (set-z80-lo! z80 #x00 u8))
    ((set-z80-b! z80 u8)  (set-z80-hi! z80 #x02 u8))
    ((set-z80-c! z80 u8)  (set-z80-lo! z80 #x02 u8))
    ((set-z80-d! z80 u8)  (set-z80-hi! z80 #x04 u8))
    ((set-z80-e! z80 u8)  (set-z80-hi! z80 #x04 u8))
    ((set-z80-h! z80 u8)  (set-z80-lo! z80 #x06 u8))
    ((set-z80-l! z80 u8)  (set-z80-hi! z80 #x06 u8)))
)
