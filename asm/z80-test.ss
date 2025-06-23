(import
  (asm lang)
  (asm z80)
  (asm z80-blocks)
  (asm asm)
  (asm asm-core))

(check-asm (ret) (db #xc9))

(check-asm
  (block
    (db 10)
    (if z
      (then
        (db 20)
        (db 21))
      (else
        (db 30)
        (db 31)))
    (db 40))
  (block
    (db 10)
    (jp z __then)
    (db 30)
    (db 31)
    (jp __end)
    (label __then)
    (db 20)
    (db 21)
    (label __end)
    (db 40)))
