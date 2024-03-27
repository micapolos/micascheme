(import
  (micascheme)
  (labs macro)
  (asm macro-z80))

(define-literals
  ; 8-bit registers
  b c d e h l a
  ixh ixl iyh iyl
  i r

  ; 16-bit registers
  pc sp
  bc de hl af
  ix iy

  ; flags
  nz z nc po pe m)

(define-matchers
  ((r $code)
    (b   #b000)
    (c   #b001)
    (d   #b010)
    (e   #b011)
    (h   #b100)
    (l   #b101)
    (a   #b111))
  ((p $p)
    (b   #b000)
    (c   #b001)
    (d   #b010)
    (e   #b011)
    (ixh #b100)
    (ixl #b101)
    (a   #b111))
  ((q $q)
    (b   #b000)
    (c   #b001)
    (d   #b010)
    (e   #b011)
    (iyh #b100)
    (iyl #b101)
    (a   #b111))
  ((c $c)
    (nz  #b000)
    (z   #b001)
    (nc  #b010)
    (c   #b011)
    (po  #b100)
    (pe  #b101)
    (p   #b110)
    (m   #b111)))

(define-macros
  ; general purpose
  ((daa)                  (db #x27))
  ((cpl)                  (db #x2f))
  ((neg)                  (db #xed) (db #x44))
  ((ccf)                  (db #x3f))
  ((scf)                  (db #x37))
  ((nop)                  (db #x00))
  ((halt)                 (db #x76))
  ((di)                   (db #xf3))
  ((ei)                   (db #xfb))

  ; 8-bit load
  ((ld a i)               (db #xed) (db #x57))
  ((ld a r)               (db #xed) (db #x5f))
  ((ld i a)               (db #xed) (db #x47))
  ((ld r a)               (db #xed) (db #x4f))
  ((ld a (bc))            (db #x0a))
  ((ld a (de))            (db #x1a))
  ((ld a ($nm))           (db #x3a) (dw $nm))
  ((ld (bc) a)            (db #x02))
  ((ld (de) a)            (db #x12))
  ((ld ($nm) a)           (db #x32) (dw $nm))
  ((ld (r $r1) (r $r2))   (db-233 #b01 $r1 $r2))
  ((ld (p $p1) (p $p2))   (db #xdd) (db-233 #b01 $p1 $p2))
  ((ld (q $q1) (q $q2))   (db #xfd) (db-233 #b01 $q1 $q2))
  ((ld (hl) (r $r))       (db-233 #b01 #b110 $r))
  ((ld (+ ix $d) (r $r))  (db #xdd) (db-233 #b01 #b110 $r) (db $d))
  ((ld (+ iy $d) (r $r))  (db #xfd) (db-233 #b01 #b110 $r) (db $d))
  ((ld (r $r) (hl))       (db-233 #b01 $r #b110))
  ((ld (r $r) (+ ix $d))  (db #xdd) (db-233 #b01 #b110 $r) (db $d))
  ((ld (r $r) (+ iy $d))  (db #xfd) (db-233 #b01 #b110 $r) (db $d))
  ((ld (r $r) $n)         (db-233 #b00 $r #b110) (db $n))
  ((ld (p $p) $n)         (db #xdd) (db-233 #b00 $p #b110))
  ((ld (q $q) $n)         (db #xfd) (db-233 #b00 $p #b110))

  ; call
  ((call $nm)             (db #xcd) (dw $nm))
  ((call (c $c) $nm)      (db-233 #b11 $c #b100) (dw $nm))
  ((ret)                  (db #xc9))
  ((ret (c $c))           (db-233 #b11 $c #b000))
  ((reti)                 (db #xed) (db #x4d))
  ((retn)                 (db #xed) (db #x45)))

; assemble some Z80 instructions
(ld a i)
(ld a r)
(ld r a)
(ld i a)

(ld a c)
(ld a d)
(ld a ixh)
(ld d 12)
(ld (+ ix 2) c)
(ld a (+ iy 12))
(ld (hl) h)
(ld a (bc))
(ld a (de))
(ld a (#x1234))
(ld (bc) a)
(ld (de) a)
(ld (#x1234) a)

(call #x1213)
(ret)