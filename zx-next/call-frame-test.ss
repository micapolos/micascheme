(import (zx-next test) (zx-next call-frame))

(define-value ret-garbage #xbbbb)

(test
  (case call-frame-load-arg-word
    (ld hl #x4433)
    (push hl)
    (ld hl #x2211)
    (push hl)
    (ld hl ret-garbage)
    (push hl)
    (call-frame
      (ld-arg hl 0)
      (assert hl #x2211)
      (ld-arg hl 2)
      (assert hl #x4433)))

  (case call-frame-load-arg-byte
    (ld hl #x4433)
    (push hl)
    (ld hl #x2211)
    (push hl)
    (ld hl ret-garbage)
    (push hl)
    (call-frame
      (ld-arg a 0)
      (assert a #x11)
      (ld-arg a 1)
      (assert a #x22)
      (ld-arg a 2)
      (assert a #x33)
      (ld-arg a 3)
      (assert a #x44)))

  (case call-frame-load-local-word
    (call-frame
      (ld hl #x6655)
      (push hl)
      (ld hl #x8877)
      (push hl)
      (ld-local hl 0)
      (assert hl #x6655)
      (ld-local hl 2)
      (assert hl #x8877)))

  (case call-frame-load-local-byte
    (call-frame
      (ld hl #x5566)
      (push hl)
      (ld hl #x7788)
      (push hl)
      (ld-local a 0)
      (assert a #x55)
      (ld-local a 1)
      (assert a #x66)
      (ld-local a 2)
      (assert a #x77)
      (ld-local a 3)
      (assert a #x88)))
)

