(import (asm lang) (asm z80) (asm z80-blocks))

(define-proc (do-10 a hl) (db 10))

(define-procs
  ((do-20 a hl) (db 20))
  ((do-30 a hl) (db 30)))

(check-asm
  (org #xc000)
  (call do-10-proc)
  (asm (start 49153) (db 10 205 0 192)))

(check-asm
  (org #xc000)
  (do-10 a hl)
  (asm (start 49153) (db 10 205 0 192)))

(check-asm
  (org #xc000)
  (do-10 20 30)
  (asm (start 49153) (db 10 62 20 33 30 0 205 0 192)))

(check-asm
  (org #xc000)
  (do-10-tc a hl)
  (asm (start 49153) (db 10 195 0 192)))

(check-asm
  (org #xc000)
  (do-10-tc 20 30)
  (asm (start 49153) (db 10 62 20 33 30 0 195 0 192)))

(check-asm
  (org #xc000)
  (call do-20-proc)
  (asm (start 49153) (db 20 205 0 192)))

(check-asm
  (org #xc000)
  (call do-30-proc)
  (asm (start 49153) (db 30 205 0 192)))

