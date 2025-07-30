(library (asm z80)
  (export
    a f b c d e h l
    af bc de hl
    ix iy
    ixh ixl iyh iyl
    pc sp
    i r

    nz z nc po pe p m

    ld

    add adc sub sbc and xor or cp
    inc dec

    bit set res

    pop push

    ex exx

    out

    jp jr djnz
    call ret reti retn rst

    daa cpl ccf scf nop halt di ei im

    cpd cpdr cpi cpir ldd lddr ldi ldir

    rla rlca rra rrca

    in

    nextreg
    mul

    break exit

    rcf)

  (import
    (asm lang))

  (define-keywords
    a f b c d e h l
    af bc de hl
    ix iy
    ixh ixl iyh iyl
    pc sp
    i r
    nz z nc po pe p m)

  (define-ops
    (keywords
      a f b c d e h l +
      af bc de hl
      ix iy
      ixh ixl iyh iyl
      pc sp
      i r
      nz z nc po pe p m)
    ; Load
    ((ld b b)          (db #b01000000))
    ((ld b c)          (db #b01000001))
    ((ld b d)          (db #b01000010))
    ((ld b e)          (db #b01000011))
    ((ld b h)          (db #b01000100))
    ((ld b l)          (db #b01000101))
    ((ld b (hl))       (db #b01000110))
    ((ld b a)          (db #b01000111))

    ((ld c b)          (db #b01001000))
    ((ld c c)          (db #b01001001))
    ((ld c d)          (db #b01001010))
    ((ld c e)          (db #b01001011))
    ((ld c h)          (db #b01001100))
    ((ld c l)          (db #b01001101))
    ((ld c (hl))       (db #b01001110))
    ((ld c a)          (db #b01001111))

    ((ld d b)          (db #b01010000))
    ((ld d c)          (db #b01010001))
    ((ld d d)          (db #b01010010))
    ((ld d e)          (db #b01010011))
    ((ld d h)          (db #b01010100))
    ((ld d l)          (db #b01010101))
    ((ld d (hl))       (db #b01010110))
    ((ld d a)          (db #b01010111))

    ((ld e b)          (db #b01011000))
    ((ld e c)          (db #b01011001))
    ((ld e d)          (db #b01011010))
    ((ld e e)          (db #b01011011))
    ((ld e h)          (db #b01011100))
    ((ld e l)          (db #b01011101))
    ((ld e (hl))       (db #b01011110))
    ((ld e a)          (db #b01011111))

    ((ld h b)          (db #b01100000))
    ((ld h c)          (db #b01100001))
    ((ld h d)          (db #b01100010))
    ((ld h e)          (db #b01100011))
    ((ld h h)          (db #b01100100))
    ((ld h l)          (db #b01100101))
    ((ld h (hl))       (db #b01100110))
    ((ld h a)          (db #b01100111))

    ((ld l b)          (db #b01101000))
    ((ld l c)          (db #b01101001))
    ((ld l d)          (db #b01101010))
    ((ld l e)          (db #b01101011))
    ((ld l h)          (db #b01101100))
    ((ld l l)          (db #b01101101))
    ((ld l (hl))       (db #b01101110))
    ((ld l a)          (db #b01101111))

    ((ld (hl) b)       (db #b01110000))
    ((ld (hl) c)       (db #b01110001))
    ((ld (hl) d)       (db #b01110010))
    ((ld (hl) e)       (db #b01110011))
    ((ld (hl) h)       (db #b01110100))
    ((ld (hl) l)       (db #b01110101))
    ((ld (hl) a)       (db #b01110111))

    ((ld a b)          (db #b01111000))
    ((ld a c)          (db #b01111001))
    ((ld a d)          (db #b01111010))
    ((ld a e)          (db #b01111011))
    ((ld a h)          (db #b01111100))
    ((ld a l)          (db #b01111101))
    ((ld a (hl))       (db #b01111110))
    ((ld a a)          (db #b01111111))

    ((ld b ixh)        (db #xdd #b01000100))
    ((ld b ixl)        (db #xdd #b01000101))
    ((ld c ixh)        (db #xdd #b01001100))
    ((ld c ixl)        (db #xdd #b01001101))
    ((ld d ixh)        (db #xdd #b01010100))
    ((ld d ixl)        (db #xdd #b01010101))
    ((ld e ixh)        (db #xdd #b01011100))
    ((ld e ixl)        (db #xdd #b01011101))
    ((ld ixh ixh)      (db #xdd #b01100100))
    ((ld ixh ixl)      (db #xdd #b01100101))
    ((ld ixl ixh)      (db #xdd #b01101100))
    ((ld ixl ixl)      (db #xdd #b01101101))
    ((ld a ixh)        (db #xdd #b01111100))
    ((ld a ixl)        (db #xdd #b01111101))

    ((ld b (+ ix d))   (db #xdd #b01000110 d))
    ((ld c (+ ix d))   (db #xdd #b01001110 d))
    ((ld d (+ ix d))   (db #xdd #b01010110 d))
    ((ld e (+ ix d))   (db #xdd #b01011110 d))
    ((ld h (+ ix d))   (db #xdd #b01100110 d))
    ((ld l (+ ix d))   (db #xdd #b01101110 d))
    ((ld a (+ ix d))   (db #xdd #b01111110 d))

    ((ld b iyh)        (db #xfd #b01000100))
    ((ld b iyl)        (db #xfd #b01000101))
    ((ld c iyh)        (db #xfd #b01001100))
    ((ld c iyl)        (db #xfd #b01001101))
    ((ld d iyh)        (db #xfd #b01010100))
    ((ld d iyl)        (db #xfd #b01010101))
    ((ld e iyh)        (db #xfd #b01011100))
    ((ld e iyl)        (db #xfd #b01011101))
    ((ld ixh iyh)      (db #xfd #b01100100))
    ((ld ixh iyl)      (db #xfd #b01100101))
    ((ld ixl iyh)      (db #xfd #b01101100))
    ((ld ixl iyl)      (db #xfd #b01101101))
    ((ld a iyh)        (db #xfd #b01111100))
    ((ld a iyl)        (db #xfd #b01111101))

    ((ld b (+ iy d))   (db #xfd #b01000110 d))
    ((ld c (+ iy d))   (db #xfd #b01001110 d))
    ((ld d (+ iy d))   (db #xfd #b01010110 d))
    ((ld e (+ iy d))   (db #xfd #b01011110 d))
    ((ld h (+ iy d))   (db #xfd #b01100110 d))
    ((ld l (+ iy d))   (db #xfd #b01101110 d))
    ((ld a (+ iy d))   (db #xfd #b01111110 d))

    ((ld (bc) a)       (db #b00000010))
    ((ld (de) a)       (db #b00010010))
    ((ld a (bc))       (db #b00001010))
    ((ld a (de))       (db #b00011010))

    ((ld i a)          (db #xed #b01000111))
    ((ld r a)          (db #xed #b01001111))
    ((ld a i)          (db #xed #b01010111))
    ((ld a r)          (db #xed #b01011111))

    ((ld sp hl)        (db #xf9))
    ((ld sp ix)        (db #xdd #xf9))
    ((ld sp iy)        (db #xdd #xf9))

    ; Load (argument)
    ((ld (nm) a)       (db #b00110010) (dw nm))
    ((ld a (nm))       (db #b00111010) (dw nm))

    ((ld b n)          (db #b00000110 n))
    ((ld c n)          (db #b00001110 n))
    ((ld d n)          (db #b00010110 n))
    ((ld e n)          (db #b00011110 n))
    ((ld h n)          (db #b00100110 n))
    ((ld l n)          (db #b00101110 n))
    ((ld (hl) n)       (db #b00110110 n))
    ((ld a n)          (db #b00111110 n))

    ((ld ixl n)        (db #xdd #b00100110 n))
    ((ld ixh n)        (db #xdd #b00101110 n))
    ((ld (+ ix d) n)   (db #xdd #b00110110 d n))

    ((ld iyl n)        (db #xfd #b00100110 n))
    ((ld iyh n)        (db #xfd #b00101110 n))
    ((ld (+ iy d) n)   (db #xfd #b00110110 d n))

    ((ld bc (nm))      (db #xed #b01001011) (dw nm))
    ((ld de (nm))      (db #xed #b01011011) (dw nm))
    ((ld hl (nm))      (db      #b00101010) (dw nm))
    ((ld ix (nm))      (db #xdd #b00101010) (dw nm))
    ((ld iy (nm))      (db #xdd #b00101010) (dw nm))
    ((ld sp (nm))      (db #xed #b01111011) (dw nm))

    ((ld (nm) bc)      (db #xed #b01000011) (dw nm))
    ((ld (nm) de)      (db #xed #b01010011) (dw nm))
    ((ld (nm) hl)      (db      #b00100010) (dw nm))
    ((ld (nm) ix)      (db #xdd #b00100010) (dw nm))
    ((ld (nm) iy)      (db #xdd #b00100010) (dw nm))
    ((ld (nm) sp)      (db #xed #b01110011) (dw nm))

    ((ld bc nm)        (db #b00000001) (dw nm))
    ((ld de nm)        (db #b00010001) (dw nm))
    ((ld hl nm)        (db #b00100001) (dw nm))
    ((ld sp nm)        (db #b00110001) (dw nm))

    ; Arithmetic and logic
    ((add b)           (db #b10000000))
    ((add c)           (db #b10000001))
    ((add d)           (db #b10000010))
    ((add e)           (db #b10000011))
    ((add h)           (db #b10000100))
    ((add l)           (db #b10000101))
    ((add (hl))        (db #b10000110))
    ((add a)           (db #b10000111))

    ((add ixh)         (db #xdd #b10000100))
    ((add ixl)         (db #xdd #b10000101))
    ((add (+ ix d))    (db #xdd #b10000110 d))

    ((add iyh)         (db #xfd #b10000100))
    ((add iyl)         (db #xfd #b10000101))
    ((add (+ iy d))    (db #xfd #b10000110 d))

    ((add n)           (db #b11000110 n))

    ((adc b)           (db #b10001000))
    ((adc c)           (db #b10001001))
    ((adc d)           (db #b10001010))
    ((adc e)           (db #b10001011))
    ((adc h)           (db #b10001100))
    ((adc l)           (db #b10001101))
    ((adc (hl))        (db #b10001110))
    ((adc a)           (db #b10001111))

    ((adc ixh)         (db #xdd #b10001100))
    ((adc ixl)         (db #xdd #b10001101))
    ((adc (+ ix d))    (db #xdd #b10001110 d))

    ((adc iyh)         (db #xfd #b10001100))
    ((adc iyl)         (db #xfd #b10001101))
    ((adc (+ iy d))    (db #xfd #b10001110 d))

    ((adc n)           (db #b11001110 n))

    ((sub b)           (db #b10010000))
    ((sub c)           (db #b10010001))
    ((sub d)           (db #b10010010))
    ((sub e)           (db #b10010011))
    ((sub h)           (db #b10010100))
    ((sub l)           (db #b10010101))
    ((sub (hl))        (db #b10010110))
    ((sub a)           (db #b10010111))

    ((sub ixh)         (db #xdd #b10010100))
    ((sub ixl)         (db #xdd #b10010101))
    ((sub (+ ix d))    (db #xdd #b10010110 d))

    ((sub iyh)         (db #xfd #b10010100))
    ((sub iyl)         (db #xfd #b10010101))
    ((sub (+ iy d))    (db #xfd #b10010110 d))

    ((sub n)           (db #b11010110 n))

    ((sbc b)           (db #b10011000))
    ((sbc c)           (db #b10011001))
    ((sbc d)           (db #b10011010))
    ((sbc e)           (db #b10011011))
    ((sbc h)           (db #b10011100))
    ((sbc l)           (db #b10011101))
    ((sbc (hl))        (db #b10011110))
    ((sbc a)           (db #b10011111))

    ((sbc ixh)         (db #xdd #b10011100))
    ((sbc ixl)         (db #xdd #b10011101))
    ((sbc (+ ix d))    (db #xdd #b10011110 d))

    ((sbc iyh)         (db #xfd #b10011100))
    ((sbc iyl)         (db #xfd #b10011101))
    ((sbc (+ iy d))    (db #xfd #b10011110 d))

    ((sbc n)           (db #b11011110 n))

    ((and b)           (db #b10100000))
    ((and c)           (db #b10100001))
    ((and d)           (db #b10100010))
    ((and e)           (db #b10100011))
    ((and h)           (db #b10100100))
    ((and l)           (db #b10100101))
    ((and (hl))        (db #b10100110))
    ((and a)           (db #b10100111))

    ((and ixh)         (db #xdd #b10100100))
    ((and ixl)         (db #xdd #b10100101))
    ((and (+ ix d))    (db #xdd #b10100110 d))

    ((and iyh)         (db #xfd #b10100100))
    ((and iyl)         (db #xfd #b10100101))
    ((and (+ iy d))    (db #xfd #b10100110 d))

    ((and n)           (db #b11100110 n))

    ((xor b)           (db #b10101000))
    ((xor c)           (db #b10101001))
    ((xor d)           (db #b10101010))
    ((xor e)           (db #b10101011))
    ((xor h)           (db #b10101100))
    ((xor l)           (db #b10101101))
    ((xor (hl))        (db #b10101110))
    ((xor a)           (db #b10101111))

    ((xor ixh)         (db #xdd #b10101100))
    ((xor ixl)         (db #xdd #b10101101))
    ((xor (+ ix d))    (db #xdd #b10101110 d))

    ((xor iyh)         (db #xfd #b10101100))
    ((xor iyl)         (db #xfd #b10101101))
    ((xor (+ iy d))    (db #xfd #b10101110 d))

    ((xor n)           (db #b11101110 n))

    ((or b)            (db #b10110000))
    ((or c)            (db #b10110001))
    ((or d)            (db #b10110010))
    ((or e)            (db #b10110011))
    ((or h)            (db #b10110100))
    ((or l)            (db #b10110101))
    ((or (hl))         (db #b10110110))
    ((or a)            (db #b10110111))

    ((or ixh)          (db #xdd #b10110100))
    ((or ixl)          (db #xdd #b10110101))
    ((or (+ ix d))     (db #xdd #b10110110 d))

    ((or iyh)          (db #xfd #b10110100))
    ((or iyl)          (db #xfd #b10110101))
    ((or (+ iy d))     (db #xfd #b10110110 d))

    ((or n)            (db #b11110110 n))

    ((cp b)            (db #b10111000))
    ((cp c)            (db #b10111001))
    ((cp d)            (db #b10111010))
    ((cp e)            (db #b10111011))
    ((cp h)            (db #b10111100))
    ((cp l)            (db #b10111101))
    ((cp (hl))         (db #b10111110))
    ((cp a)            (db #b10111111))

    ((cp ixh)          (db #xdd #b10111100))
    ((cp ixl)          (db #xdd #b10111101))
    ((cp (+ ix d))     (db #xdd #b10111110 d))

    ((cp iyh)          (db #xfd #b10111100))
    ((cp iyl)          (db #xfd #b10111101))
    ((cp (+ iy d))     (db #xfd #b10111110 d))

    ((cp n)            (db #b11111110 n))

    ((inc b)           (db #b00000100))
    ((inc c)           (db #b00001100))
    ((inc d)           (db #b00010100))
    ((inc e)           (db #b00011100))
    ((inc h)           (db #b00100100))
    ((inc l)           (db #b00101100))
    ((inc (hl))        (db #b00110100))
    ((inc a)           (db #b00111100))

    ((inc ixh)         (db #xdd #b00100100))
    ((inc ixl)         (db #xdd #b00101100))
    ((inc (+ ix d))    (db #xdd #b00110100 d))

    ((inc iyh)         (db #xfd #b00100100))
    ((inc iyl)         (db #xfd #b00101100))
    ((inc (+ iy d))    (db #xfd #b00110100 d))

    ((dec b)           (db #b00000101))
    ((dec c)           (db #b00001101))
    ((dec d)           (db #b00010101))
    ((dec e)           (db #b00011101))
    ((dec h)           (db #b00100101))
    ((dec l)           (db #b00101101))
    ((dec (hl))        (db #b00110101))
    ((dec a)           (db #b00111101))

    ((dec ixh)         (db #xdd #b00100101))
    ((dec ixl)         (db #xdd #b00101101))
    ((dec (+ ix d))    (db #xdd #b00110101 d))

    ((dec iyh)         (db #xfd #b00100101))
    ((dec iyl)         (db #xfd #b00101101))
    ((dec (+ iy d))    (db #xfd #b00110101 d))

    ; 16-bit arithmetic
    ((add hl bc)       (db #b00001001))
    ((add hl de)       (db #b00011001))
    ((add hl hl)       (db #b00101001))
    ((add hl sp)       (db #b00111001))

    ((add ix bc)       (db #xdd #b00001001))
    ((add ix de)       (db #xdd #b00011001))
    ((add ix ix)       (db #xdd #b00101001))
    ((add ix sp)       (db #xdd #b00111001))

    ((add iy bc)       (db #xfd #b00001001))
    ((add iy de)       (db #xfd #b00011001))
    ((add iy iy)       (db #xfd #b00101001))
    ((add iy sp)       (db #xfd #b00111001))

    ; Next extension
    ((add hl a)        (db #xed #b00110001))
    ((add de a)        (db #xed #b00110010))
    ((add bc a)        (db #xed #b00110011))

    ((add hl nm)       (db #xed #b00110100) (dw nm))
    ((add de nm)       (db #xed #b00110101) (dw nm))
    ((add bc nm)       (db #xed #b00110110) (dw nm))

    ((inc bc)          (db      #b00000011))
    ((inc de)          (db      #b00010011))
    ((inc hl)          (db      #b00100011))
    ((inc ix)          (db #xdd #b00100011))
    ((inc iy)          (db #xfd #b00100011))
    ((inc sp)          (db      #b00110011))

    ((dec bc)          (db      #b00001011))
    ((dec de)          (db      #b00011011))
    ((dec hl)          (db      #b00101011))
    ((dec ix)          (db #xdd #b00101011))
    ((dec iy)          (db #xfd #b00101011))
    ((dec sp)          (db      #b00111011))

    ; Bit
    ((bit 0 b)         (db      #xcb   #b01000000))
    ((bit 0 c)         (db      #xcb   #b01000001))
    ((bit 0 d)         (db      #xcb   #b01000010))
    ((bit 0 e)         (db      #xcb   #b01000011))
    ((bit 0 h)         (db      #xcb   #b01000100))
    ((bit 0 l)         (db      #xcb   #b01000101))
    ((bit 0 (hl))      (db      #xcb   #b01000110))
    ((bit 0 (+ ix d))  (db #xdd #xcb d #b01000110))
    ((bit 0 (+ iy d))  (db #xfd #xcb d #b01000110))
    ((bit 0 a)         (db      #xcb   #b01000111))

    ((bit 1 b)         (db      #xcb   #b01001000))
    ((bit 1 c)         (db      #xcb   #b01001001))
    ((bit 1 d)         (db      #xcb   #b01001010))
    ((bit 1 e)         (db      #xcb   #b01001011))
    ((bit 1 h)         (db      #xcb   #b01001100))
    ((bit 1 l)         (db      #xcb   #b01001101))
    ((bit 1 (hl))      (db      #xcb   #b01001110))
    ((bit 1 (+ ix d))  (db #xdd #xcb d #b01001110))
    ((bit 1 (+ iy d))  (db #xfd #xcb d #b01001110))
    ((bit 1 a)         (db      #xcb   #b01001111))

    ((bit 2 b)         (db      #xcb   #b01010000))
    ((bit 2 c)         (db      #xcb   #b01010001))
    ((bit 2 d)         (db      #xcb   #b01010010))
    ((bit 2 e)         (db      #xcb   #b01010011))
    ((bit 2 h)         (db      #xcb   #b01010100))
    ((bit 2 l)         (db      #xcb   #b01010101))
    ((bit 2 (hl))      (db      #xcb   #b01010110))
    ((bit 2 (+ ix d))  (db #xdd #xcb d #b01010110))
    ((bit 2 (+ iy d))  (db #xfd #xcb d #b01010110))
    ((bit 2 a)         (db      #xcb   #b01010111))

    ((bit 3 b)         (db      #xcb   #b01011000))
    ((bit 3 c)         (db      #xcb   #b01011001))
    ((bit 3 d)         (db      #xcb   #b01011010))
    ((bit 3 e)         (db      #xcb   #b01011011))
    ((bit 3 h)         (db      #xcb   #b01011100))
    ((bit 3 l)         (db      #xcb   #b01011101))
    ((bit 3 (hl))      (db      #xcb   #b01011110))
    ((bit 3 (+ ix d))  (db #xdd #xcb d #b01011110))
    ((bit 3 (+ iy d))  (db #xfd #xcb d #b01011110))
    ((bit 3 a)         (db      #xcb   #b01011111))

    ((bit 4 b)         (db      #xcb   #b01100000))
    ((bit 4 c)         (db      #xcb   #b01100001))
    ((bit 4 d)         (db      #xcb   #b01100010))
    ((bit 4 e)         (db      #xcb   #b01100011))
    ((bit 4 h)         (db      #xcb   #b01100100))
    ((bit 4 l)         (db      #xcb   #b01100101))
    ((bit 4 (hl))      (db      #xcb   #b01100110))
    ((bit 4 (+ ix d))  (db #xdd #xcb d #b01100110))
    ((bit 4 (+ iy d))  (db #xfd #xcb d #b01100110))
    ((bit 4 a)         (db      #xcb   #b01100111))

    ((bit 5 b)         (db      #xcb   #b01101000))
    ((bit 5 c)         (db      #xcb   #b01101001))
    ((bit 5 d)         (db      #xcb   #b01101010))
    ((bit 5 e)         (db      #xcb   #b01101011))
    ((bit 5 h)         (db      #xcb   #b01101100))
    ((bit 5 l)         (db      #xcb   #b01101101))
    ((bit 5 (hl))      (db      #xcb   #b01101110))
    ((bit 5 (+ ix d))  (db #xdd #xcb d #b01101110))
    ((bit 5 (+ iy d))  (db #xfd #xcb d #b01101110))
    ((bit 5 a)         (db      #xcb   #b01101111))

    ((bit 6 b)         (db      #xcb   #b01110000))
    ((bit 6 c)         (db      #xcb   #b01110001))
    ((bit 6 d)         (db      #xcb   #b01110010))
    ((bit 6 e)         (db      #xcb   #b01110011))
    ((bit 6 h)         (db      #xcb   #b01110100))
    ((bit 6 l)         (db      #xcb   #b01110101))
    ((bit 6 (hl))      (db      #xcb   #b01110110))
    ((bit 6 (+ ix d))  (db #xdd #xcb d #b01110110))
    ((bit 6 (+ iy d))  (db #xfd #xcb d #b01110110))
    ((bit 6 a)         (db      #xcb   #b01110111))

    ((bit 7 b)         (db      #xcb   #b01111000))
    ((bit 7 c)         (db      #xcb   #b01111001))
    ((bit 7 d)         (db      #xcb   #b01111010))
    ((bit 7 e)         (db      #xcb   #b01111011))
    ((bit 7 h)         (db      #xcb   #b01111100))
    ((bit 7 l)         (db      #xcb   #b01111101))
    ((bit 7 (hl))      (db      #xcb   #b01111110))
    ((bit 7 (+ ix d))  (db #xdd #xcb d #b01111110))
    ((bit 7 (+ iy d))  (db #xfd #xcb d #b01111110))
    ((bit 7 a)         (db      #xcb   #b01111111))

    ((set 0 b)         (db      #xcb   #b11000000))
    ((set 0 c)         (db      #xcb   #b11000001))
    ((set 0 d)         (db      #xcb   #b11000010))
    ((set 0 e)         (db      #xcb   #b11000011))
    ((set 0 h)         (db      #xcb   #b11000100))
    ((set 0 l)         (db      #xcb   #b11000101))
    ((set 0 (hl))      (db      #xcb   #b11000110))
    ((set 0 (+ ix d))  (db #xdd #xcb d #b11000110))
    ((set 0 (+ iy d))  (db #xfd #xcb d #b11000110))
    ((set 0 a)         (db      #xcb   #b11000111))

    ((set 1 b)         (db      #xcb   #b11001000))
    ((set 1 c)         (db      #xcb   #b11001001))
    ((set 1 d)         (db      #xcb   #b11001010))
    ((set 1 e)         (db      #xcb   #b11001011))
    ((set 1 h)         (db      #xcb   #b11001100))
    ((set 1 l)         (db      #xcb   #b11001101))
    ((set 1 (hl))      (db      #xcb   #b11001110))
    ((set 1 (+ ix d))  (db #xdd #xcb d #b11001110))
    ((set 1 (+ iy d))  (db #xfd #xcb d #b11001110))
    ((set 1 a)         (db      #xcb   #b11001111))

    ((set 2 b)         (db      #xcb   #b11010000))
    ((set 2 c)         (db      #xcb   #b11010001))
    ((set 2 d)         (db      #xcb   #b11010010))
    ((set 2 e)         (db      #xcb   #b11010011))
    ((set 2 h)         (db      #xcb   #b11010100))
    ((set 2 l)         (db      #xcb   #b11010101))
    ((set 2 (hl))      (db      #xcb   #b11010110))
    ((set 2 (+ ix d))  (db #xdd #xcb d #b11010110))
    ((set 2 (+ iy d))  (db #xfd #xcb d #b11010110))
    ((set 2 a)         (db      #xcb   #b11010111))

    ((set 3 b)         (db      #xcb   #b11011000))
    ((set 3 c)         (db      #xcb   #b11011001))
    ((set 3 d)         (db      #xcb   #b11011010))
    ((set 3 e)         (db      #xcb   #b11011011))
    ((set 3 h)         (db      #xcb   #b11011100))
    ((set 3 l)         (db      #xcb   #b11011101))
    ((set 3 (hl))      (db      #xcb   #b11011110))
    ((set 3 (+ ix d))  (db #xdd #xcb d #b11011110))
    ((set 3 (+ iy d))  (db #xfd #xcb d #b11011110))
    ((set 3 a)         (db      #xcb   #b11011111))

    ((set 4 b)         (db      #xcb   #b11100000))
    ((set 4 c)         (db      #xcb   #b11100001))
    ((set 4 d)         (db      #xcb   #b11100010))
    ((set 4 e)         (db      #xcb   #b11100011))
    ((set 4 h)         (db      #xcb   #b11100100))
    ((set 4 l)         (db      #xcb   #b11100101))
    ((set 4 (hl))      (db      #xcb   #b11100110))
    ((set 4 (+ ix d))  (db #xdd #xcb d #b11100110))
    ((set 4 (+ iy d))  (db #xfd #xcb d #b11100110))
    ((set 4 a)         (db      #xcb   #b11100111))

    ((set 5 b)         (db      #xcb   #b11101000))
    ((set 5 c)         (db      #xcb   #b11101001))
    ((set 5 d)         (db      #xcb   #b11101010))
    ((set 5 e)         (db      #xcb   #b11101011))
    ((set 5 h)         (db      #xcb   #b11101100))
    ((set 5 l)         (db      #xcb   #b11101101))
    ((set 5 (hl))      (db      #xcb   #b11101110))
    ((set 5 (+ ix d))  (db #xdd #xcb d #b11101110))
    ((set 5 (+ iy d))  (db #xfd #xcb d #b11101110))
    ((set 5 a)         (db      #xcb   #b11101111))

    ((set 6 b)         (db      #xcb   #b11110000))
    ((set 6 c)         (db      #xcb   #b11110001))
    ((set 6 d)         (db      #xcb   #b11110010))
    ((set 6 e)         (db      #xcb   #b11110011))
    ((set 6 h)         (db      #xcb   #b11110100))
    ((set 6 l)         (db      #xcb   #b11110101))
    ((set 6 (hl))      (db      #xcb   #b11110110))
    ((set 6 (+ ix d))  (db #xdd #xcb d #b11110110))
    ((set 6 (+ iy d))  (db #xfd #xcb d #b11110110))
    ((set 6 a)         (db      #xcb   #b11110111))

    ((set 7 b)         (db      #xcb   #b11111000))
    ((set 7 c)         (db      #xcb   #b11111001))
    ((set 7 d)         (db      #xcb   #b11111010))
    ((set 7 e)         (db      #xcb   #b11111011))
    ((set 7 h)         (db      #xcb   #b11111100))
    ((set 7 l)         (db      #xcb   #b11111101))
    ((set 7 (hl))      (db      #xcb   #b11111110))
    ((set 7 (+ ix d))  (db #xdd #xcb d #b11111110))
    ((set 7 (+ iy d))  (db #xfd #xcb d #b11111110))
    ((set 7 a)         (db      #xcb   #b11111111))

    ((res 0 b)         (db      #xcb   #b10000000))
    ((res 0 c)         (db      #xcb   #b10000001))
    ((res 0 d)         (db      #xcb   #b10000010))
    ((res 0 e)         (db      #xcb   #b10000011))
    ((res 0 h)         (db      #xcb   #b10000100))
    ((res 0 l)         (db      #xcb   #b10000101))
    ((res 0 (hl))      (db      #xcb   #b10000110))
    ((res 0 (+ ix d))  (db #xdd #xcb d #b10000110))
    ((res 0 (+ iy d))  (db #xfd #xcb d #b10000110))
    ((res 0 a)         (db      #xcb   #b10000111))

    ((res 1 b)         (db      #xcb   #b10001000))
    ((res 1 c)         (db      #xcb   #b10001001))
    ((res 1 d)         (db      #xcb   #b10001010))
    ((res 1 e)         (db      #xcb   #b10001011))
    ((res 1 h)         (db      #xcb   #b10001100))
    ((res 1 l)         (db      #xcb   #b10001101))
    ((res 1 (hl))      (db      #xcb   #b10001110))
    ((res 1 (+ ix d))  (db #xdd #xcb d #b10001110))
    ((res 1 (+ iy d))  (db #xfd #xcb d #b10001110))
    ((res 1 a)         (db      #xcb   #b10001111))

    ((res 2 b)         (db      #xcb   #b10010000))
    ((res 2 c)         (db      #xcb   #b10010001))
    ((res 2 d)         (db      #xcb   #b10010010))
    ((res 2 e)         (db      #xcb   #b10010011))
    ((res 2 h)         (db      #xcb   #b10010100))
    ((res 2 l)         (db      #xcb   #b10010101))
    ((res 2 (hl))      (db      #xcb   #b10010110))
    ((res 2 (+ ix d))  (db #xdd #xcb d #b10010110))
    ((res 2 (+ iy d))  (db #xfd #xcb d #b10010110))
    ((res 2 a)         (db      #xcb   #b10010111))

    ((res 3 b)         (db      #xcb   #b10011000))
    ((res 3 c)         (db      #xcb   #b10011001))
    ((res 3 d)         (db      #xcb   #b10011010))
    ((res 3 e)         (db      #xcb   #b10011011))
    ((res 3 h)         (db      #xcb   #b10011100))
    ((res 3 l)         (db      #xcb   #b10011101))
    ((res 3 (hl))      (db      #xcb   #b10011110))
    ((res 3 (+ ix d))  (db #xdd #xcb d #b10011110))
    ((res 3 (+ iy d))  (db #xfd #xcb d #b10011110))
    ((res 3 a)         (db      #xcb   #b10011111))

    ((res 4 b)         (db      #xcb   #b10100000))
    ((res 4 c)         (db      #xcb   #b10100001))
    ((res 4 d)         (db      #xcb   #b10100010))
    ((res 4 e)         (db      #xcb   #b10100011))
    ((res 4 h)         (db      #xcb   #b10100100))
    ((res 4 l)         (db      #xcb   #b10100101))
    ((res 4 (hl))      (db      #xcb   #b10100110))
    ((res 4 (+ ix d))  (db #xdd #xcb d #b10100110))
    ((res 4 (+ iy d))  (db #xfd #xcb d #b10100110))
    ((res 4 a)         (db      #xcb   #b10100111))

    ((res 5 b)         (db      #xcb   #b10101000))
    ((res 5 c)         (db      #xcb   #b10101001))
    ((res 5 d)         (db      #xcb   #b10101010))
    ((res 5 e)         (db      #xcb   #b10101011))
    ((res 5 h)         (db      #xcb   #b10101100))
    ((res 5 l)         (db      #xcb   #b10101101))
    ((res 5 (hl))      (db      #xcb   #b10101110))
    ((res 5 (+ ix d))  (db #xdd #xcb d #b10101110))
    ((res 5 (+ iy d))  (db #xfd #xcb d #b10101110))
    ((res 5 a)         (db      #xcb   #b10101111))

    ((res 6 b)         (db      #xcb   #b10110000))
    ((res 6 c)         (db      #xcb   #b10110001))
    ((res 6 d)         (db      #xcb   #b10110010))
    ((res 6 e)         (db      #xcb   #b10110011))
    ((res 6 h)         (db      #xcb   #b10110100))
    ((res 6 l)         (db      #xcb   #b10110101))
    ((res 6 (hl))      (db      #xcb   #b10110110))
    ((res 6 (+ ix d))  (db #xdd #xcb d #b10110110))
    ((res 6 (+ iy d))  (db #xfd #xcb d #b10110110))
    ((res 6 a)         (db      #xcb   #b10110111))

    ((res 7 b)         (db      #xcb   #b10111000))
    ((res 7 c)         (db      #xcb   #b10111001))
    ((res 7 d)         (db      #xcb   #b10111010))
    ((res 7 e)         (db      #xcb   #b10111011))
    ((res 7 h)         (db      #xcb   #b10111100))
    ((res 7 l)         (db      #xcb   #b10111101))
    ((res 7 (hl))      (db      #xcb   #b10111110))
    ((res 7 (+ ix d))  (db #xdd #xcb d #b10111110))
    ((res 7 (+ iy d))  (db #xfd #xcb d #b10111110))
    ((res 7 a)         (db      #xcb   #b10111111))

    ; General purpose
    ((daa)             (db #x27))
    ((cpl)             (db #x2f))
    ((ccf)             (db #x3f))
    ((scf)             (db #x37))
    ((nop)             (db #x00))
    ((halt)            (db #x76))
    ((di)              (db #xf3))
    ((ei)              (db #xfb))
    ((neg)             (db #xed #x44))
    ((im 0)            (db #xed #x46))
    ((im 1)            (db #xed #x56))
    ((im 2)            (db #xed #x5e))

    ; Stack
    ((pop bc)          (db #b11000001))
    ((pop de)          (db #b11010001))
    ((pop hl)          (db #b11100001))
    ((pop af)          (db #b11110001))

    ((pop ix)          (db #xdd #b11100001))
    ((pop iy)          (db #xfd #b11100001))

    ((push bc)         (db #b11000101))
    ((push de)         (db #b11010101))
    ((push hl)         (db #b11100101))
    ((push af)         (db #b11110101))

    ((push ix)         (db #xdd #b11100101))
    ((push iy)         (db #xfd #b11100101))

    ; Exchange
    ((ex af)           (db #x08))
    ((ex de hl)        (db #xeb))
    ((ex (sp) hl)      (db #xe3))
    ((ex (sp) ix)      (db #xdd #xe3))
    ((ex (sp) iy)      (db #xfd #xe3))
    ((exx)             (db #xd9))

    ; Block / transfer
    ((cpd)             (db #xed #xa9))
    ((cpdr)            (db #xed #xb9))
    ((cpi)             (db #xed #xa1))
    ((cpir)            (db #xed #xb1))
    ((ldd)             (db #xed #xa8))
    ((lddr)            (db #xed #xb8))
    ((ldi)             (db #xed #xa0))
    ((ldir)            (db #xed #xb0))

    ; Shift/rotate
    ((rla)             (db #x17))
    ((rlca)            (db #x07))
    ((rra)             (db #x1f))
    ((rrca)            (db #x0f))

    ; Output
    ((out (n) a)       (db #xd3 n))

    ; Jump
    ((jp (hl))         (db #xe9))
    ((jp (ix))         (db #xdd #xe9))
    ((jp (iy))         (db #xfd #xe9))
    ((djnz nm)         (db #x10) (db-e nm))

    ; Jump (argument)
    ((jp nm)           (db #xc3) (dw nm))
    ((jp nz nm)        (db #b11000010) (dw nm))
    ((jp z nm)         (db #b11001010) (dw nm))
    ((jp nc nm)        (db #b11010010) (dw nm))
    ((jp c nm)         (db #b11011010) (dw nm))
    ((jp po nm)        (db #b11100010) (dw nm))
    ((jp pe nm)        (db #b11101010) (dw nm))
    ((jp p nm)         (db #b11110010) (dw nm))
    ((jp m nm)         (db #b11111010) (dw nm))

    ((jr nm)           (db #b00011000) (db-e nm))
    ((jr nz nm)        (db #b00100000) (db-e nm))
    ((jr z nm)         (db #b00101000) (db-e nm))
    ((jr nc nm)        (db #b00110000) (db-e nm))
    ((jr c nm)         (db #b00111000) (db-e nm))

    ; Call and return
    ((call nm)         (db #xcd) (dw nm))
    ((call nz nm)      (db #b11000100) (dw nm))
    ((call z nm)       (db #b11001100) (dw nm))
    ((call nc nm)      (db #b11010100) (dw nm))
    ((call c nm)       (db #b11011100) (dw nm))
    ((call po nm)      (db #b11100100) (dw nm))
    ((call pe nm)      (db #b11101100) (dw nm))
    ((call p nm)       (db #b11110100) (dw nm))
    ((call m nm)       (db #b11111100) (dw nm))

    ((ret)             (db #xc9))

    ((ret nz)          (db #b11000000))
    ((ret z)           (db #b11001000))
    ((ret nc)          (db #b11010000))
    ((ret c)           (db #b11011000))
    ((ret po)          (db #b11100000))
    ((ret pe)          (db #b11101000))
    ((ret p)           (db #b11110000))
    ((ret m)           (db #b11111000))

    ((reti)            (db #xed #x4d))
    ((retn)            (db #xed #x45))

    ((rst #x00)        (db #b11000111))
    ((rst #x08)        (db #b11001111))
    ((rst #x10)        (db #b11010111))
    ((rst #x18)        (db #b11011111))
    ((rst #x20)        (db #b11100111))
    ((rst #x28)        (db #b11101111))
    ((rst #x30)        (db #b11110111))
    ((rst #x38)        (db #b11111111))

    ; Input/output
    ((in b (c))        (db #xed #b01000000))
    ((in c (c))        (db #xed #b01001000))
    ((in d (c))        (db #xed #b01010000))
    ((in e (c))        (db #xed #b01011000))
    ((in h (c))        (db #xed #b01100000))
    ((in l (c))        (db #xed #b01101000))
    ((in (c))          (db #xed #b01110000))
    ((in a (c))        (db #xed #b01111000))

    ; Next
    ((nextreg n a)     (db #xed #x92 n))
    ((nextreg n n2)    (db #xed #x91 n n2))
    ((mul d e)         (db #xed #x30))

    ; Helpers
    ((rcf)             (or a))

    ; CSpect
    ((break)           (db #xfd #x00))
    ((exit)            (db #xdd #x00))
  )
)
