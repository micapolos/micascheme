(library (zx-next sdcc-1)
  (export
    ld-byte ld-word ld-dword
    byte-inc word-inc
    byte-dec word-dec
    byte-add word-add
    byte-sub word-sub
    byte-mul

    byte-write word-write dword-write
    byte-assert word-assert dword-assert)
  (import (zx-next core) (zx-next write) (zx-next assert))

  ; Op for SDCC-1 calling conventions.
  (define-ops
    ((ld-byte n)        (ld a n))
    ((ld-word nn)       (ld de nn))
    ((ld-word hi lo)    (ld de (fxior (fxsll hi 8) lo)))
    ((ld-dword nnnn)    (ld hl (fxsrl nnnn 16)) (ld de (fxand #xffff nnnn)))
    ((ld-dword nn mm)   (ld hl nn) (ld de mm))

    ((byte-inc)      (inc a))
    ((word-inc)      (inc hl) (ex de hl))
    ((byte-dec)      (dec a))
    ((word-dec)      (dec hl) (ex de hl))

    ((byte-add)      (add l))
    ((word-add)      (add hl de) (ex de hl))
    ((byte-sub)      (add l))
    ((word-sub)      (sub hl de) (ex de hl))
    ((byte-mul)      (ld d a) (ld e l) (mul d e))

    ((byte->word)    (ld d 0) (ld e a))
    ((word->dword)   (ld de 0))

    ((word->hi)      (ld a h))
    ((word->lo)      (ld a l))

    ((dword->hi))
    ((dword->lo)     (ex de hl))

    ((when-byte-zero? body ...)  (or a) (when z body ...))
    ((when-word-zero? body ...)  (ld a h) (or l) (when z body ...))
    ((when-byte=? body ...)      (or a l) (when z body ...))
    ((when-word=? body ...)      (rcf) (sub hl de) (when z body ...))

    ((if-byte-zero? then else)  (or a) (if z then else))
    ((if-word-zero? then else)  (ld a h) (or l) (if z then else))
    ((if-byte=? then else)      (or a l) (if z then else))
    ((if-word=? then else)      (rcf) (sub hl de) (if then else)))

  (define-ops
    ((byte-write)  (call write-byte))
    ((word-write)  (call write-word))
    ((dword-write)
      (preserve (de) (call write-word))
      (ex de hl)
      (call write-word)))

  (define-ops
    ((byte-assert n)      (assert a n))
    ((word-assert nn)     (assert de nn))
    ((dword-assert nnnn)
      (assert hl (fxsrl nnnn 16))
      (assert de (fxand #xffff nnnn))))
)
