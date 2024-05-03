(library (emu z80 asm)
  (export
    org
    asm db dw nop ld jp out halt
    a f b c d e h l)
  (import (scheme) (syntax) (syntaxes) (throw) (emu reg) (emu math))

  (define param (make-thread-parameter (lambda ($addr $u8) (throw no-asm))))

  (define-aux-keywords a f b c d e h l)
  (define-reg-16 org)
  (define-rules-syntaxes (literals a f b c d e h l)
    ((asm mem) (param (lambda (addr u8) (mem addr u8))))
    ((db n) (begin ((param) (org) (u8 n)) (org (u16+1 (org)))))
    ((dw nm) (let (($u16 nm)) (db (u16-l $u16)) (db (u16-h $u16))))
    ((nop) (db 0))
    ((halt) (db #b01110110))
    ((ld b a) (begin (db #b01000111)))
    ((ld c a) (begin (db #b01001111)))
    ((ld (nm) a) (begin (db #b00110010) (dw nm)))
    ((ld a (nm)) (begin (db #b00111010) (dw nm)))
    ((ld a n) (begin (db #b00111110) (db n)))
    ((out (n) a) (begin (db #b11010011) (db n)))
    ((jp nm) (begin (db #b11000011) (dw nm))))
)
