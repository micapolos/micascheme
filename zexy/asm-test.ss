(import (check) (zexy asm))

(define-syntax-rule (check-op $syntax ($op ...))
  (check
    (equal?
      (asm-bytevector
        (asm-op (empty-asm) #'$syntax))
      (u8-list->bytevector
        (list $op ...)))))

(check-op (db) ())
(check-op (db #x12) (#x12))
(check-op (db #\a) (97))
(check-op (db "a ") (97 32))
(check-op (db #x12 #\a " ") (#x12 97 32))

(check-op (dz) (0))
(check-op (dz #x12) (#x12 0))
(check-op (dz #\a) (97 0))
(check-op (dz "a ") (97 32 0))
(check-op (dz #x12 #\a " ") (#x12 97 32 0))

(check-op (dw) ())
(check-op (dw #x1234) (#x34 #x12))
(check-op (dw #x1234 #x5678) (#x34 #x12 #x78 #x56))

(check-op (ld b c) (#b01000001))
(check-op (ld d e) (#b01010011))
(check-op (ld h l) (#b01100101))
(check-op (ld a b) (#b01111000))

(check-op (ld b #x13) (#b00000110 #x13))
(check-op (ld l #x13) (#b00101110 #x13))
(check-op (ld a #x13) (#b00111110 #x13))

(check-op (ld b (hl)) (#b01000110))
(check-op (ld l (hl)) (#b01101110))
(check-op (ld a (hl)) (#b01111110))

(check-op (ld (hl) b) (#b01110000))
(check-op (ld (hl) l) (#b01110101))
(check-op (ld (hl) a) (#b01110111))

(check-op (ld (hl) #x13) (#b00110110 #x13))

(check-op (ld a (bc)) (#b00001010))
(check-op (ld a (de)) (#b00011010))
(check-op (ld a (#x1234)) (#b00111010 #x34 #x12))

(check-op (ld (bc) a) (#b00000010))
(check-op (ld (de) a) (#b00010010))
(check-op (ld (#x1234) a) (#b00110010 #x34 #x12))

(check-op (ld bc #x1234) (#b00000001 #x34 #x12))
(check-op (ld de #x1234) (#b00010001 #x34 #x12))
(check-op (ld hl #x1234) (#b00100001 #x34 #x12))
(check-op (ld sp #x1234) (#b00110001 #x34 #x12))

(check-op (ld bc (#x1234)) (#xed #b01001011 #x34 #x12))
(check-op (ld de (#x1234)) (#xed #b01011011 #x34 #x12))
(check-op (ld hl (#x1234)) (#x2a #x34 #x12))
(check-op (ld sp (#x1234)) (#xed #b01111011 #x34 #x12))

(check-op (ld (#x1234) bc) (#xed #b01000011 #x34 #x12))
(check-op (ld (#x1234) de) (#xed #b01010011 #x34 #x12))
(check-op (ld (#x1234) hl) (#x22 #x34 #x12))
(check-op (ld (#x1234) sp) (#xed #b01110011 #x34 #x12))

(check-op (ld sp hl) (#xf9))

(check-op (add b) (#b10000000))
(check-op (add c) (#b10000001))
(check-op (add d) (#b10000010))
(check-op (add e) (#b10000011))
(check-op (add h) (#b10000100))
(check-op (add l) (#b10000101))
(check-op (add (hl)) (#b10000110))
(check-op (add a) (#b10000111))

(check-op (add b) (#b10000000))
(check-op (adc b) (#b10001000))
(check-op (sub b) (#b10010000))
(check-op (sbc b) (#b10011000))
(check-op (and b) (#b10100000))
(check-op (xor b) (#b10101000))
(check-op (or b) (#b10110000))
(check-op (cp b) (#b10111000))

(check-op (add #x12) (#b11000110 #x12))
(check-op (adc #x12) (#b11001110 #x12))
(check-op (sub #x12) (#b11010110 #x12))
(check-op (sbc #x12) (#b11011110 #x12))
(check-op (and #x12) (#b11100110 #x12))
(check-op (xor #x12) (#b11101110 #x12))
(check-op (or #x12) (#b11110110 #x12))
(check-op (cp #x12) (#b11111110 #x12))

(check-op (inc b) (#b00000100))
(check-op (inc c) (#b00001100))
(check-op (inc d) (#b00010100))
(check-op (inc e) (#b00011100))
(check-op (inc h) (#b00100100))
(check-op (inc l) (#b00101100))
(check-op (inc (hl)) (#b00110100))
(check-op (inc a) (#b00111100))

(check-op (dec b) (#b00000101))
(check-op (dec c) (#b00001101))
(check-op (dec d) (#b00010101))
(check-op (dec e) (#b00011101))
(check-op (dec h) (#b00100101))
(check-op (dec l) (#b00101101))
(check-op (dec (hl)) (#b00110101))
(check-op (dec a) (#b00111101))

(check-op (inc bc) (#b00000011))
(check-op (inc de) (#b00010011))
(check-op (inc hl) (#b00100011))
(check-op (inc sp) (#b00110011))

(check-op (dec bc) (#b00001011))
(check-op (dec de) (#b00011011))
(check-op (dec hl) (#b00101011))
(check-op (dec sp) (#b00111011))

(check-op (add hl bc) (#b00001001))
(check-op (add hl de) (#b00011001))
(check-op (add hl hl) (#b00101001))
(check-op (add hl sp) (#b00111001))

(check-op (adc hl bc) (#xed #b01001010))
(check-op (adc hl de) (#xed #b01011010))
(check-op (adc hl hl) (#xed #b01101010))
(check-op (adc hl sp) (#xed #b01111010))

(check-op (sbc hl bc) (#xed #b01000010))
(check-op (sbc hl de) (#xed #b01010010))
(check-op (sbc hl hl) (#xed #b01100010))
(check-op (sbc hl sp) (#xed #b01110010))

(check-op (call #x1234) (#b11001101 #x34 #x12))

(check-op (call nz #x1234) (#b11000100 #x34 #x12))
(check-op (call c #x1234) (#b11011100 #x34 #x12))
(check-op (call m #x1234) (#b11111100 #x34 #x12))

(check-op (ret) (#b11001001))

(check-op (ret nz) (#b11000000))
(check-op (ret c) (#b11011000))
(check-op (ret m) (#b11111000))

(check-op (rst #x0) (#b11000111))
(check-op (rst #x8) (#b11001111))
(check-op (rst #x38) (#b11111111))

(check-op (jp #x1234) (#b11000011 #x34 #x12))

(check-op (jp (hl)) (#b11101001))

(check-op (jp nz #x1234) (#b11000010 #x34 #x12))
(check-op (jp c #x1234) (#b11011010 #x34 #x12))
(check-op (jp m #x1234) (#b11111010 #x34 #x12))

(check-op (djnz 2) (#x10 #x02))
(check-op (djnz -2) (#x10 #xfe))

(check-op (push bc) (#b11000101))
(check-op (push de) (#b11010101))
(check-op (push hl) (#b11100101))
(check-op (push af) (#b11110101))

(check-op (push #x1234) (#xed #x8a #x34 #x12))

(check-op (pop bc) (#b11000001))
(check-op (pop de) (#b11010001))
(check-op (pop hl) (#b11100001))
(check-op (pop af) (#b11110001))

(check-op (daa) (#x27))
(check-op (cpl) (#x2f))
(check-op (neg) (#xed #x44))
(check-op (ccf) (#x3f))
(check-op (scf) (#x37))
(check-op (nop) (#x00))
(check-op (halt) (#x76))
(check-op (di) (#xf3))
(check-op (ei) (#xfb))
(check-op (im 0) (#xed #x46))
(check-op (im 1) (#xed #x56))
(check-op (im 2) (#xed #x5e))

(check-op (ex af af2) (#x08))
(check-op (ex de hl) (#xeb))
(check-op (ex (sp) hl) (#xe3))

(check-op (exx) (#xd9))

(check-op (bit 0 b) (#xcb #b01000000))
(check-op (bit 1 b) (#xcb #b01001000))
(check-op (bit 2 b) (#xcb #b01010000))
(check-op (bit 3 b) (#xcb #b01011000))
(check-op (bit 4 b) (#xcb #b01100000))
(check-op (bit 5 b) (#xcb #b01101000))
(check-op (bit 6 b) (#xcb #b01110000))
(check-op (bit 7 b) (#xcb #b01111000))

(check-op (bit 2 b) (#xcb #b01010000))
(check-op (bit 2 c) (#xcb #b01010001))
(check-op (bit 2 d) (#xcb #b01010010))
(check-op (bit 2 e) (#xcb #b01010011))
(check-op (bit 2 h) (#xcb #b01010100))
(check-op (bit 2 l) (#xcb #b01010101))
(check-op (bit 2 (hl)) (#xcb #b01010110))
(check-op (bit 2 a) (#xcb #b01010111))

(check-op (set 2 (hl)) (#xcb #b11010110))
(check-op (res 2 (hl)) (#xcb #b10010110))

(check-op (rlc b) (#xcb #b00000000))
(check-op (rlc c) (#xcb #b00000001))
(check-op (rlc d) (#xcb #b00000010))
(check-op (rlc e) (#xcb #b00000011))
(check-op (rlc h) (#xcb #b00000100))
(check-op (rlc l) (#xcb #b00000101))
(check-op (rlc (hl)) (#xcb #b00000110))
(check-op (rlc a) (#xcb #b00000111))

(check-op (rrc b) (#xcb #b00001000))
(check-op (rrc c) (#xcb #b00001001))
(check-op (rrc d) (#xcb #b00001010))
(check-op (rrc e) (#xcb #b00001011))
(check-op (rrc h) (#xcb #b00001100))
(check-op (rrc l) (#xcb #b00001101))
(check-op (rrc (hl)) (#xcb #b00001110))
(check-op (rrc a) (#xcb #b00001111))

(check-op (rl b) (#xcb #b00010000))
(check-op (rl c) (#xcb #b00010001))
(check-op (rl d) (#xcb #b00010010))
(check-op (rl e) (#xcb #b00010011))
(check-op (rl h) (#xcb #b00010100))
(check-op (rl l) (#xcb #b00010101))
(check-op (rl (hl)) (#xcb #b00010110))
(check-op (rl a) (#xcb #b00010111))

(check-op (rr b) (#xcb #b00011000))
(check-op (rr c) (#xcb #b00011001))
(check-op (rr d) (#xcb #b00011010))
(check-op (rr e) (#xcb #b00011011))
(check-op (rr h) (#xcb #b00011100))
(check-op (rr l) (#xcb #b00011101))
(check-op (rr (hl)) (#xcb #b00011110))
(check-op (rr a) (#xcb #b00011111))

(check-op (sla b) (#xcb #b00100000))
(check-op (sla c) (#xcb #b00100001))
(check-op (sla d) (#xcb #b00100010))
(check-op (sla e) (#xcb #b00100011))
(check-op (sla h) (#xcb #b00100100))
(check-op (sla l) (#xcb #b00100101))
(check-op (sla (hl)) (#xcb #b00100110))
(check-op (sla a) (#xcb #b00100111))

(check-op (sra b) (#xcb #b00101000))
(check-op (sra c) (#xcb #b00101001))
(check-op (sra d) (#xcb #b00101010))
(check-op (sra e) (#xcb #b00101011))
(check-op (sra h) (#xcb #b00101100))
(check-op (sra l) (#xcb #b00101101))
(check-op (sra (hl)) (#xcb #b00101110))
(check-op (sra a) (#xcb #b00101111))

(check-op (sli b) (#xcb #b00110000))
(check-op (sli c) (#xcb #b00110001))
(check-op (sli d) (#xcb #b00110010))
(check-op (sli e) (#xcb #b00110011))
(check-op (sli h) (#xcb #b00110100))
(check-op (sli l) (#xcb #b00110101))
(check-op (sli (hl)) (#xcb #b00110110))
(check-op (sli a) (#xcb #b00110111))

(check-op (srl b) (#xcb #b00111000))
(check-op (srl c) (#xcb #b00111001))
(check-op (srl d) (#xcb #b00111010))
(check-op (srl e) (#xcb #b00111011))
(check-op (srl h) (#xcb #b00111100))
(check-op (srl l) (#xcb #b00111101))
(check-op (srl (hl)) (#xcb #b00111110))
(check-op (srl a) (#xcb #b00111111))

(check-op (rla) (#x017))
(check-op (rlca) (#x07))
(check-op (rra) (#x1f))
(check-op (rrca) (#x0f))

(check-op (rld) (#xed #x6f))
(check-op (rrd) (#xed #x67))

(check-op (cpd) (#xed #xa9))
(check-op (cpdr) (#xed #xb9))
(check-op (cpi) (#xed #xa1))
(check-op (cpir) (#xed #xb1))
(check-op (ldd) (#xed #xa8))
(check-op (lddr) (#xed #xb8))
(check-op (ldi) (#xed #xa0))
(check-op (ldir) (#xed #xb0))

(check-op (in a (#x12)) (#xdb #x12))

(check-op (in b (c)) (#xed #b01000000))
(check-op (in c (c)) (#xed #b01001000))
(check-op (in d (c)) (#xed #b01010000))
(check-op (in e (c)) (#xed #b01011000))
(check-op (in h (c)) (#xed #b01100000))
(check-op (in l (c)) (#xed #b01101000))
(check-op (in (c))   (#xed #b01110000))
(check-op (in a (c)) (#xed #b01111000))

(check-op (ind) (#xed #xaa))
(check-op (indr) (#xed #xba))
(check-op (ini) (#xed #xa2))
(check-op (inir) (#xed #xb2))

(check-op (out (#x12) a) (#xd3 #x12))

(check-op (out (c) b) (#xed #b01000001))
(check-op (out (c) c) (#xed #b01001001))
(check-op (out (c) d) (#xed #b01010001))
(check-op (out (c) e) (#xed #b01011001))
(check-op (out (c) h) (#xed #b01100001))
(check-op (out (c) l) (#xed #b01101001))
(check-op (out (c) 0) (#xed #b01110001))
(check-op (out (c) a) (#xed #b01111001))

(check-op (outi) (#xed #xa3))
(check-op (otir) (#xed #xb3))
(check-op (outd) (#xed #xab))
(check-op (otdr) (#xed #xbb))

(check-op (add hl a) (#xed #b00110001))
(check-op (add de a) (#xed #b00110010))
(check-op (add bc a) (#xed #b00110011))

(check-op (add hl #x1234) (#xed #b00110100 #x34 #x12))
(check-op (add de #x1234) (#xed #b00110101 #x34 #x12))
(check-op (add bc #x1234) (#xed #b00110110 #x34 #x12))

(check-op (swapnib) (#xed #x23))

(check-op (nextreg #x12 a) (#xed #x92 #x12))
(check-op (nextreg #x12 #x34) (#xed #x91 #x12 #x34))

; ---------------------------------------------------

(define-syntax-rule (check-ops $syntax ... ($u8s ...))
  (check
    (equal?
      (asm-bytevector
        (asm-ops (empty-asm) (list #'$syntax ...)))
      (u8-list->bytevector
        (list $u8s ...)))))

(check-ops
  (val foo #x12)
  (db foo)
  (#x12))

(check-ops
  (val foo #x12)
  (val bar #x34)
  (db (+ foo bar))
  (#x46))

(check-ops
  (val foo #x12)
  (val bar #x34)
  (val goo (+ foo bar))
  (db goo)
  (#x46))

(check-ops
  (val foo #x12)
  (val bar #x34)
  (db (+ foo bar))
  (#x46))

(check-ops
  (val foo #x12)
  (val bar #x34)
  (dw (+ foo bar))
  (#x46 #x00))

(check-ops
  foo
  bar
  ())

(check-ops
  foo (db #x12 #x34)
  (#x12 #x34))

(check-ops
  foo (dw foo)
  bar (dw bar)
  foo (dw foo)
  (#x04 #x00 #x02 #x00 #x04 #x00))

(check-ops
  (org #x1200)
  foo (dw foo)
  bar (dw bar)
  foo (dw foo)
  (#x04 #x12 #x02 #x12 #x04 #x12))

(check-ops
  (call foo)
  foo (ret)
  (#xcd #x03 #x00 #xc9))

(check-ops
  (org #x1200)
  (call foo)
  foo (ret)
  (#xcd #x03 #x12 #xc9))

(check-ops
  loop (djnz loop)
  (#x10 #xfe))

(check-ops
  (djnz loop)
  loop
  (#x10 #x00))

; === local

(check-ops
  (val foo #x12)
  (db foo)
  (local
    (val foo #x34)
    (db foo))
  (db foo)
  (#x12 #x34 #x12))

; === block

(check-ops
  (org #x2000)
  (val foo #x12)
  (db foo)
  (block goo
    (val foo #x34)
    (db foo))
  (db foo)
  (dw goo)
  (#x12 #x34 #x12 #x01 #x20))

; import

(check-ops
  (val bar #x34)
  (import "zexy/asm-test-imported.asm.ss")
  (db bar)
  (#x12 #x34))

(check-ops
  (val bar #x34)
  (import "zexy/asm-test-imported.asm.ss")
  (import "zexy/asm-test-imported.asm.ss")
  (db bar)
  (#x12 #x34))
