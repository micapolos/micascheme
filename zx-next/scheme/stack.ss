(library (zx-next scheme stack)
  (export
    stack-data
    stack-init
    stack-push
    stack-pop
    stack-preserve)
  (import (zx-next core))

  ; Why ds does not work?
  (define-fragment stack-data
    (db 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (db 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (db 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (db 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (db 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (db 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (db 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (db 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (db 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (db 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (db 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (db 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (db 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (db 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (db 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (db 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

  (define-fragment stack-init
    (output (de stack-top))
    (ld de (+ stack-data 256))
    (ret))

  (define-fragment stack-push
    (input
      (de stack-top)
      (hla value))
    (output
      (de stack-top))
    (ex de hl)
    (dec hl)
    (ld (hl) a)
    (dec hl)
    (ld (hl) d)
    (dec hl)
    (ld (hl) e)
    (ex de hl)
    (ret))

  (define-fragment stack-pop
    (input
      (de stack-top))
    (output
      (de stack-top)
      (hla value))
    (ex de hl)
    (ld e (hl))
    (inc hl)
    (ld d (hl))
    (inc hl)
    (ld a (hl))
    (inc hl)
    (ex de hl)
    (ret))

  (define-op (stack-preserve body ...)
    (preserve (de) body ...))
)
