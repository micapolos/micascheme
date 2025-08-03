(import (zx-next demo) (zx-next lookup))

(define-asm db-pot
  (db
    #x01 #x02 #x04 #x08
    #x10 #x20 #x40 #x80))

(define-asm dw-pot
  (dw
    #x0001 #x0002 #x0004 #x0008
    #x0010 #x0020 #x0040 #x0080
    #x0100 #x0200 #x0400 #x0800
    #x1000 #x2000 #x4000 #x8000))

(demo
  (ld hl db-pot)
  (ld a 3)
  (call lookup-byte)
  (writeln "1^3 = " a)

  (ld hl dw-pot)
  (ld a 14)
  (call lookup-word)
  (writeln "1^14 = " de))
