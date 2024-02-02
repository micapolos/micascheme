(import (check) (zexy asm))

(define-syntax-rule (check-op $syntax ($u8s ...))
  (check
    (equal?
      (asm-bytevector
        (asm-op (empty-asm) #'$syntax))
      (u8-list->bytevector
        (list $u8s ...)))))

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

(check-op (djnz #x12) (#b00010000 #x12))
(check-op (djnz #xfe) (#b00010000 #xfe))

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

; ---------------------------------------------------

(define-syntax-rule (check-ops $syntax ... ($u8s ...))
  (check
    (equal?
      (asm-bytevector
        (asm-ops (empty-asm) (list #'$syntax ...)))
      (u8-list->bytevector
        (list $u8s ...)))))

(check-ops
  (nop)
  (ret)
  (#x0 #xc9))
