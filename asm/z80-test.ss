(import
  (prefix (micascheme) %)
  (asm z80)
  (asm z80-keywords))

(%define-rule-syntax (check-reg (id prefix? r3 offset?) %...)
  (%begin
    (%check-equal?
      (%pattern-match? (%syntax id) (reg $prefix? $r3 $offset?)
        (%list
          (%lift? %syntax->datum $prefix?)
          $r3
          (%lift? %syntax->datum $offset?)))
      (%list
        (%or (%datum prefix?) (%quote prefix?))
        r3
        (%or (%datum offset?) (%quote offset?))))
    %...))

(check-reg
  (b           #f         #b000  #f    )
  (c           #f         #b001  #f    )
  (d           #f         #b010  #f    )
  (e           #f         #b011  #f    )
  (h           #f         #b100  #f    )
  (l           #f         #b101  #f    )
  ((hl)        #f         #b110  #f    )
  (a           #f         #b111  #f    )
  (ixh         (db #xdd)  #b100  #f    )
  (ixl         (db #xdd)  #b101  #f    )
  ((+ ix d)    (db #xdd)  #b110  (db d))
  (iyh         (db #xfd)  #b100  #f    )
  (iyl         (db #xfd)  #b101  #f    )
  ((+ iy d)    (db #xfd)  #b110  (db d)))

; =====================================================================

(%define-rule-syntax (check-asm (op out %...) %...)
   (%begin
     (%check-datum=?
       (%list->syntax (op->asm? (%syntax op)))
       (%syntax (out %...)))
     %...))

(check-asm
  ((add a b)          (db #b10000000))
  ((add a c)          (db #b10000001))
  ((add a d)          (db #b10000010))
  ((add a e)          (db #b10000011))
  ((add a h)          (db #b10000100))
  ((add a l)          (db #b10000101))
  ((add a (hl))       (db #b10000110))
  ((add a a)          (db #b10000111))

  ((add a ixh)        (db #xdd) (db #b10000100))
  ((add a ixl)        (db #xdd) (db #b10000101))
  ((add a iyh)        (db #xfd) (db #b10000100))
  ((add a iyl)        (db #xfd) (db #b10000101))
  ((add a (+ ix d))   (db #xdd) (db #b10000110) (db d))
  ((add a (+ iy d))   (db #xfd) (db #b10000110) (db d))

  ((add a n)          (db #b11000110) (db n))

  ((adc a b)          (db #b10001000))
  ((adc a c)          (db #b10001001))
  ((adc a d)          (db #b10001010))
  ((adc a e)          (db #b10001011))
  ((adc a h)          (db #b10001100))
  ((adc a l)          (db #b10001101))
  ((adc a (hl))       (db #b10001110))
  ((adc a a)          (db #b10001111))

  ((adc a ixh)        (db #xdd) (db #b10001100))
  ((adc a ixl)        (db #xdd) (db #b10001101))
  ((adc a iyh)        (db #xfd) (db #b10001100))
  ((adc a iyl)        (db #xfd) (db #b10001101))
  ((adc a (+ ix d))   (db #xdd) (db #b10001110) (db d))
  ((adc a (+ iy d))   (db #xfd) (db #b10001110) (db d))

  ((adc a n)          (db #b11001110) (db n))

  ((sub a b)          (db #b10010000))
  ((sub a c)          (db #b10010001))
  ((sub a d)          (db #b10010010))
  ((sub a e)          (db #b10010011))
  ((sub a h)          (db #b10010100))
  ((sub a l)          (db #b10010101))
  ((sub a (hl))       (db #b10010110))
  ((sub a a)          (db #b10010111))

  ((sub a ixh)        (db #xdd) (db #b10010100))
  ((sub a ixl)        (db #xdd) (db #b10010101))
  ((sub a iyh)        (db #xfd) (db #b10010100))
  ((sub a iyl)        (db #xfd) (db #b10010101))
  ((sub a (+ ix d))   (db #xdd) (db #b10010110) (db d))
  ((sub a (+ iy d))   (db #xfd) (db #b10010110) (db d))

  ((sub a n)          (db #b11010110) (db n))

  ((sbc a b)          (db #b10011000))
  ((sbc a c)          (db #b10011001))
  ((sbc a d)          (db #b10011010))
  ((sbc a e)          (db #b10011011))
  ((sbc a h)          (db #b10011100))
  ((sbc a l)          (db #b10011101))
  ((sbc a (hl))       (db #b10011110))
  ((sbc a a)          (db #b10011111))

  ((sbc a ixh)        (db #xdd) (db #b10011100))
  ((sbc a ixl)        (db #xdd) (db #b10011101))
  ((sbc a iyh)        (db #xfd) (db #b10011100))
  ((sbc a iyl)        (db #xfd) (db #b10011101))
  ((sbc a (+ ix d))   (db #xdd) (db #b10011110) (db d))
  ((sbc a (+ iy d))   (db #xfd) (db #b10011110) (db d))

  ((sbc a n)          (db #b11011110) (db n))

  ((and b)            (db #b10100000))
  ((and c)            (db #b10100001))
  ((and d)            (db #b10100010))
  ((and e)            (db #b10100011))
  ((and h)            (db #b10100100))
  ((and l)            (db #b10100101))
  ((and (hl))         (db #b10100110))
  ((and a)            (db #b10100111))

  ((and ixh)          (db #xdd) (db #b10100100))
  ((and ixl)          (db #xdd) (db #b10100101))
  ((and iyh)          (db #xfd) (db #b10100100))
  ((and iyl)          (db #xfd) (db #b10100101))
  ((and (+ ix d))     (db #xdd) (db #b10100110) (db d))
  ((and (+ iy d))     (db #xfd) (db #b10100110) (db d))

  ((and n)            (db #b11100110) (db n))

  ((or b)             (db #b10101000))
  ((or c)             (db #b10101001))
  ((or d)             (db #b10101010))
  ((or e)             (db #b10101011))
  ((or h)             (db #b10101100))
  ((or l)             (db #b10101101))
  ((or (hl))          (db #b10101110))
  ((or a)             (db #b10101111))

  ((or ixh)           (db #xdd) (db #b10101100))
  ((or ixl)           (db #xdd) (db #b10101101))
  ((or iyh)           (db #xfd) (db #b10101100))
  ((or iyl)           (db #xfd) (db #b10101101))
  ((or (+ ix d))      (db #xdd) (db #b10101110) (db d))
  ((or (+ iy d))      (db #xfd) (db #b10101110) (db d))

  ((or n)             (db #b11101110) (db n))

  ((xor b)            (db #b10110000))
  ((xor c)            (db #b10110001))
  ((xor d)            (db #b10110010))
  ((xor e)            (db #b10110011))
  ((xor h)            (db #b10110100))
  ((xor l)            (db #b10110101))
  ((xor (hl))         (db #b10110110))
  ((xor a)            (db #b10110111))

  ((xor ixh)          (db #xdd) (db #b10110100))
  ((xor ixl)          (db #xdd) (db #b10110101))
  ((xor iyh)          (db #xfd) (db #b10110100))
  ((xor iyl)          (db #xfd) (db #b10110101))
  ((xor (+ ix d))     (db #xdd) (db #b10110110) (db d))
  ((xor (+ iy d))     (db #xfd) (db #b10110110) (db d))

  ((xor n)            (db #b11110110) (db n))

  ((cp b)             (db #b10111000))
  ((cp c)             (db #b10111001))
  ((cp d)             (db #b10111010))
  ((cp e)             (db #b10111011))
  ((cp h)             (db #b10111100))
  ((cp l)             (db #b10111101))
  ((cp (hl))          (db #b10111110))
  ((cp a)             (db #b10111111))

  ((cp ixh)           (db #xdd) (db #b10111100))
  ((cp ixl)           (db #xdd) (db #b10111101))
  ((cp iyh)           (db #xfd) (db #b10111100))
  ((cp iyl)           (db #xfd) (db #b10111101))
  ((cp (+ ix d))      (db #xdd) (db #b10111110) (db d))
  ((cp (+ iy d))      (db #xfd) (db #b10111110) (db d))

  ((cp n)             (db #b11111110) (db n))

  ((inc b)            (db #b00000100))
  ((inc c)            (db #b00001100))
  ((inc d)            (db #b00010100))
  ((inc e)            (db #b00011100))
  ((inc h)            (db #b00100100))
  ((inc l)            (db #b00101100))
  ((inc (hl))         (db #b00110100))
  ((inc a)            (db #b00111100))

  ((inc ixh)          (db #xdd) (db #b00100100))
  ((inc ixl)          (db #xdd) (db #b00101100))
  ((inc iyh)          (db #xfd) (db #b00100100))
  ((inc iyl)          (db #xfd) (db #b00101100))
  ((inc (+ ix d))     (db #xdd) (db #b00110100) (db d))
  ((inc (+ iy d))     (db #xfd) (db #b00110100) (db d))

  ((dec b)            (db #b00000101))
  ((dec c)            (db #b00001101))
  ((dec d)            (db #b00010101))
  ((dec e)            (db #b00011101))
  ((dec h)            (db #b00100101))
  ((dec l)            (db #b00101101))
  ((dec (hl))         (db #b00110101))
  ((dec a)            (db #b00111101))

  ((dec ixh)          (db #xdd) (db #b00100101))
  ((dec ixl)          (db #xdd) (db #b00101101))
  ((dec iyh)          (db #xfd) (db #b00100101))
  ((dec iyl)          (db #xfd) (db #b00101101))
  ((dec (+ ix d))     (db #xdd) (db #b00110101) (db d))
  ((dec (+ iy d))     (db #xfd) (db #b00110101) (db d))

  ((ld b b)           (db #b01000000))
  ((ld b c)           (db #b01000001))
  ((ld b d)           (db #b01000010))
  ((ld b e)           (db #b01000011))
  ((ld b h)           (db #b01000100))
  ((ld b l)           (db #b01000101))
  ((ld b (hl))        (db #b01000110))
  ((ld b a)           (db #b01000111))

  ((ld c b)           (db #b01001000))
  ((ld c c)           (db #b01001001))
  ((ld c d)           (db #b01001010))
  ((ld c e)           (db #b01001011))
  ((ld c h)           (db #b01001100))
  ((ld c l)           (db #b01001101))
  ((ld c (hl))        (db #b01001110))
  ((ld c a)           (db #b01001111))

  ((ld d b)           (db #b01010000))
  ((ld d c)           (db #b01010001))
  ((ld d d)           (db #b01010010))
  ((ld d e)           (db #b01010011))
  ((ld d h)           (db #b01010100))
  ((ld d l)           (db #b01010101))
  ((ld d (hl))        (db #b01010110))
  ((ld d a)           (db #b01010111))

  ((ld e b)           (db #b01011000))
  ((ld e c)           (db #b01011001))
  ((ld e d)           (db #b01011010))
  ((ld e e)           (db #b01011011))
  ((ld e h)           (db #b01011100))
  ((ld e l)           (db #b01011101))
  ((ld e (hl))        (db #b01011110))
  ((ld e a)           (db #b01011111))

  ((ld h b)           (db #b01100000))
  ((ld h c)           (db #b01100001))
  ((ld h d)           (db #b01100010))
  ((ld h e)           (db #b01100011))
  ((ld h h)           (db #b01100100))
  ((ld h l)           (db #b01100101))
  ((ld h (hl))        (db #b01100110))
  ((ld h a)           (db #b01100111))

  ((ld l b)           (db #b01101000))
  ((ld l c)           (db #b01101001))
  ((ld l d)           (db #b01101010))
  ((ld l e)           (db #b01101011))
  ((ld l h)           (db #b01101100))
  ((ld l l)           (db #b01101101))
  ((ld l (hl))        (db #b01101110))
  ((ld l a)           (db #b01101111))

  ((ld (hl) b)        (db #b01110000))
  ((ld (hl) c)        (db #b01110001))
  ((ld (hl) d)        (db #b01110010))
  ((ld (hl) e)        (db #b01110011))
  ((ld (hl) h)        (db #b01110100))
  ((ld (hl) l)        (db #b01110101))
  ((halt)             (db #b01110110))
  ((ld (hl) a)        (db #b01110111))

  ((ld a b)           (db #b01111000))
  ((ld a c)           (db #b01111001))
  ((ld a d)           (db #b01111010))
  ((ld a e)           (db #b01111011))
  ((ld a h)           (db #b01111100))
  ((ld a l)           (db #b01111101))
  ((ld a (hl))        (db #b01111110))
  ((ld a a)           (db #b01111111))

  ((ld b ixh)         (db #xdd) (db #b01000100))
  ((ld b ixl)         (db #xdd) (db #b01000101))
  ((ld b (+ ix d))    (db #xdd) (db #b01000110) (db d))
  ((ld b iyh)         (db #xfd) (db #b01000100))
  ((ld b iyl)         (db #xfd) (db #b01000101))
  ((ld b (+ iy d))    (db #xfd) (db #b01000110) (db d))

  ((ld c ixh)         (db #xdd) (db #b01001100))
  ((ld c ixl)         (db #xdd) (db #b01001101))
  ((ld c (+ ix d))    (db #xdd) (db #b01001110) (db d))
  ((ld c iyh)         (db #xfd) (db #b01001100))
  ((ld c iyl)         (db #xfd) (db #b01001101))
  ((ld c (+ iy d))    (db #xfd) (db #b01001110) (db d))

  ((ld d ixh)         (db #xdd) (db #b01010100))
  ((ld d ixl)         (db #xdd) (db #b01010101))
  ((ld d (+ ix d))    (db #xdd) (db #b01010110) (db d))
  ((ld d iyh)         (db #xfd) (db #b01010100))
  ((ld d iyl)         (db #xfd) (db #b01010101))
  ((ld d (+ iy d))    (db #xfd) (db #b01010110) (db d))

  ((ld e ixh)         (db #xdd) (db #b01011100))
  ((ld e ixl)         (db #xdd) (db #b01011101))
  ((ld e (+ ix d))    (db #xdd) (db #b01011110) (db d))
  ((ld e iyh)         (db #xfd) (db #b01011100))
  ((ld e iyl)         (db #xfd) (db #b01011101))
  ((ld e (+ iy d))    (db #xfd) (db #b01011110) (db d))

  ((ld ixh ixh)       (db #xdd) (db #b01100100))
  ((ld ixh ixl)       (db #xdd) (db #b01100101))
  ((ld ixh (+ ix d))  (db #xdd) (db #b01100110) (db d))
  ((ld iyh iyh)       (db #xfd) (db #b01100100))
  ((ld iyh iyl)       (db #xfd) (db #b01100101))
  ((ld iyh (+ iy d))  (db #xfd) (db #b01100110) (db d))

  ((ld ixl ixh)       (db #xdd) (db #b01101100))
  ((ld ixl ixl)       (db #xdd) (db #b01101101))
  ((ld ixl (+ ix d))  (db #xdd) (db #b01101110) (db d))
  ((ld iyl iyh)       (db #xfd) (db #b01101100))
  ((ld iyl iyl)       (db #xfd) (db #b01101101))
  ((ld iyl (+ iy d))  (db #xfd) (db #b01101110) (db d))

  ((ld a ixh)         (db #xdd) (db #b01111100))
  ((ld a ixl)         (db #xdd) (db #b01111101))
  ((ld a (+ ix d))    (db #xdd) (db #b01111110) (db d))
  ((ld a iyh)         (db #xfd) (db #b01111100))
  ((ld a iyl)         (db #xfd) (db #b01111101))
  ((ld a (+ iy d))    (db #xfd) (db #b01111110) (db d))

  ((ld b n)           (db #b00000110) (db n))
  ((ld c n)           (db #b00001110) (db n))
  ((ld d n)           (db #b00010110) (db n))
  ((ld e n)           (db #b00011110) (db n))
  ((ld h n)           (db #b00100110) (db n))
  ((ld l n)           (db #b00101110) (db n))
  ((ld (hl) n)        (db #b00110110) (db n))
  ((ld a n)           (db #b00111110) (db n))

  ((ld ixh n)         (db #xdd) (db #b00100110) (db n))
  ((ld ixl n)         (db #xdd) (db #b00101110) (db n))
  ((ld iyh n)         (db #xfd) (db #b00100110) (db n))
  ((ld iyl n)         (db #xfd) (db #b00101110) (db n))
  ((ld (+ ix d) n)    (db #xdd) (db #b00110110) (db d) (db n))
  ((ld (+ iy d) n)    (db #xfd) (db #b00110110) (db d) (db n))

  ((ld ixh ixh)       (db #xdd) (db #b01100100))
  ((ld ixh ixl)       (db #xdd) (db #b01100101))
  ((ld ixl ixh)       (db #xdd) (db #b01101100))
  ((ld ixl ixl)       (db #xdd) (db #b01101101))

  ((ld iyh iyh)       (db #xfd) (db #b01100100))
  ((ld iyh iyl)       (db #xfd) (db #b01100101))
  ((ld iyl iyh)       (db #xfd) (db #b01101100))
  ((ld iyl iyl)       (db #xfd) (db #b01101101))

  ((ld a (bc))        (db #b00001010))
  ((ld a (de))        (db #b00011010))
  ((ld a (nm))        (db #b00111010) (dw nm))
  ((ld (bc) a)        (db #b00000010))
  ((ld (de) a)        (db #b00010010))
  ((ld (nm) a)        (db #b00110010) (dw nm))

  ((ld a i)           (db #xed) (db #x57))
  ((ld a r)           (db #xed) (db #x5f))
  ((ld i a)           (db #xed) (db #x47))
  ((ld r a)           (db #xed) (db #x4f))
)
