(import
  (zx-next demo)
  (zx-next call-frame))

(define-asm my-proc
  (call-frame
    (ld hl #x5566)
    (push hl)
    (ld hl #x7788)
    (push hl)

    (ld-local a 0)
    (writeln "Local byte 0: " a)
    (ld-local a 1)
    (writeln "Local byte 1: " a)
    (ld-local a 2)
    (writeln "Local byte 2: " a)
    (ld-local a 3)
    (writeln "Local byte 3: " a)

    (ld-local de 0)
    (writeln "Local word 0: " de)
    (ld-local de 2)
    (writeln "Local word 2: " de)

    (ld-arg a 0)
    (writeln "Arg byte 0: " a)
    (ld-arg a 1)
    (writeln "Arg byte 1: " a)
    (ld-arg a 2)
    (writeln "Arg byte 2: " a)
    (ld-arg a 3)
    (writeln "Arg byte 3: " a)

    (ld-arg de 0)
    (writeln "Arg word 0: " de)
    (ld-arg de 2)
    (writeln "Arg word 2: " de)

    (pop hl)
    (pop hl))
  (ret))

(demo
  (ld hl #x1122)
  (push hl)
  (ld hl #x3344)
  (push hl)

  (call my-proc)

  (pop hl)
  (pop hl))
