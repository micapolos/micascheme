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

    (assert bc #x1234)
    (assert de #x5678)
    (assert hl #x9abc)

    (scf) (assert c)
    (rcf) (assert nc)
    (xor a) (assert z)
    (cp 1) (assert nz)

    ; failures
    (catch (assert a #xff))
    (catch (assert b #xff))
    (catch (assert c #xff))
    (catch (assert d #xff))
    (catch (assert e #xff))
    (catch (assert h #xff))
    (catch (assert l #xff))
    (catch (assert bc #xffff))
    (catch (assert de #xffff))
    (catch (assert hl #xffff))
    (catch (scf) (assert nc))
    (catch (scf) (ccf) (assert c))
    (catch (xor a) (assert nz))
    (catch (cp 1) (assert z))

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
