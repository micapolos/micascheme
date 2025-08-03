(import (zx-next demo) (zx-next assert) (zx-next regs) (zx-next throw))

(define-asm expected-regs   (dw #x0011 #x2233 #x4455 #x6677 #x8899 #xaabb #xccdd #xeeff #x1234 #x5678))
(define-asm actual-regs     (dw #x0111 #x2233 #x4455 #x6377 #x8899 #xaabb #xcc1d #xeefd #x1234 #x5278))
(define-asm mask-regs       (dw #xffff #xffff #xffff #xffff #xffff #xffff #xffff #x0000 #x0000 #x0000))

(demo
  (catch
    (writeln "Testing asserts...")
    (ld a #x10)
    (ld bc #x1234)
    (ld de #x5678)
    (ld hl #x9abc)

    (assert a #x10)
    (assert b #x12)
    (assert c #x34)
    (assert d #x56)
    (assert e #x78)
    (assert h #x9a)
    (assert l #xbc)
    (break)
    (catch (assert l #xbd))
    (break)

    (writeln "Testing equal regs...")
    (ld hl expected-regs)
    (ld de expected-regs)
    (ld bc mask-regs)
    (call assert-regs)

    (writeln "Testing non equal regs...")
    (ld hl expected-regs)
    (ld de actual-regs)
    (ld bc mask-regs)
    (call assert-regs)

    (writeln-error "Should not be here")))
