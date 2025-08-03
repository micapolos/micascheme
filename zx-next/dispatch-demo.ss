(import (zx-next demo) (zx-next dispatch))

(define-fragments
  (writeln-zero  (writeln "zero") (ret))
  (writeln-one   (writeln "one") (ret))
  (writeln-two   (writeln "two") (ret))
  (writeln-three (writeln "three") (ret)))

(define-asm test-dispatch-jp
  (dispatch-jp
    writeln-zero
    writeln-one
    writeln-two
    writeln-three))

(define-asm test-ret-dispatch
  (ret-dispatch
    (writeln "zero")
    (writeln "one")
    (writeln "two")
    (writeln "four")))

(demo
  (writeln "Testing inline dispatch...")
  (ld a 1)
  (dispatch
    (writeln "zero")
    (writeln "one")
    (writeln "two")
    (writeln "four"))

  (writeln "Testing dispatch-call...")
  (ld a 1)
  (dispatch-call
    writeln-zero
    writeln-one
    writeln-two
    writeln-three)

  (writeln "Testing dispatch-jp...")
  (ld a 1)
  (call test-dispatch-jp)

  (writeln "Testing ret-dispatch...")
  (ld a 1)
  (call test-ret-dispatch)

  (writeln "Testing tail-dispatch...")
  (ld a 1)
  (tail-dispatch (jp jp-dispatch-done)
    (writeln "zero")
    (writeln "one")
    (writeln "two")
    (writeln "four"))
  jp-dispatch-done
)
