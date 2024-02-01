(import (check) (zexy asm))

(define-syntax-rule (check-op $syntax ($u8s ...))
  (check
    (equal?
      (asm-bytevector
        (asm-op (empty-asm) #'$syntax))
      (u8-list->bytevector
        (list $u8s ...)))))

(check-op (nop) (#x0))

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
