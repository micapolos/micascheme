(library (zx-next allocation)
  (export
    bytes-allocation
    pointers-allocation

    write-allocation
    write-allocation-tc
    write-allocation-proc)
  (import
    (zx-next core)
    (zx-next write)
    (zx-next banked-pointer))

  (comment
    (allocation starts with 1 byte header followed by data)
    (the header has the following structure
      (bit 7 : 0 = bytes / 1 = banked pointers)
      (bit 6 ... 0 : 0 = number of items)))

  (define-ops
    ((bytes-allocation x ...)
      (db (length x ...))
      (db x) ...)
    ((pointers-allocation (bank address) ...)
      (db (fxior #x80 (length (bank address) ...)))
      (begin (db bank) (dw address)) ...))

  (define-proc (write-bytes a hl)
    (ld b 0)
    (ld c a)
    (jp write-mem))

  (define-proc (write-pointers a hl)
    (ld b a)
    (loop-djnz
      (preserve (bc)
        (ld a (hl))
        (inc hl)
        (ld e (hl))
        (inc hl)
        (ld d (hl))
        (inc hl)
        (preserve (hl)
          (ex de hl)
          (ld e a)
          (write-banked-pointer ehl)
          (write #\space))))
    (writeln)
    (ret))

  (define-proc (write-allocation hl)
    (ld a (hl))
    (inc hl)
    (bit 7 a)
    (when z (write-bytes-tc a hl))
    (res 7 a)
    (write-pointers-tc a hl))
)
