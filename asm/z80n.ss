(library (asm z80n)
  (export
    label db dw

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

    pop push

    ex exx

    out

    jp djnz
    call ret reti retn rst

    daa cpl ccf scf nop halt di ei im

    loop-jp loop-djnz

    dup

    run)
  (import
    (rename (micascheme)
      (run %run)
      (and %and)
      (or %or)
      (xor %xor)
      (push %push)
      (pop %pop))
    (asm core)
    (nex)
    (cspect)
    (asm expression)
    (rename (asm fragment) (db %db) (dw %dw))
    (rename (asm frame) (label %label)))

  (export (import (only (micascheme) +)))

  (define-ops
    (label %label)
    (db %db)
    (dw %dw))

  (define-keywords
    a f b c d e h l
    af bc de hl
    ix iy
    ixh ixl iyh iyl
    pc sp
    i r
    nz z nc po pe p m)

  (define-rules-syntaxes
    (literals
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
    ((ld (de) a)       (db #b00000110))
    ((ld a (bc))       (db #b00001010))
    ((ld a (de))       (db #b00001110))

    ((ld i a)          (db #xed #b01000111))
    ((ld r a)          (db #xed #b01001111))
    ((ld a i)          (db #xed #b01010111))
    ((ld a r)          (db #xed #b01011111))

    ((ld sp hl)        (db #xf9))
    ((ld sp ix)        (db #xdd #xf9))
    ((ld sp iy)        (db #xdd #xf9))

    ; Load (argument)
    ((ld b n)          (db #b00000110 n))
    ((ld c n)          (db #b00001110 n))
    ((ld d n)          (db #b00010110 n))
    ((ld e n)          (db #b00011110 n))
    ((ld h n)          (db #b00100110 n))
    ((ld l n)          (db #b00101110 n))
    ((ld (hl) n)       (db #b00110110 n))
    ((ld a n)          (db #b00111110 n))

    ((ld (nm) a)       (begin (db #b00110010) (dw nm)))
    ((ld a (nm))       (begin (db #b00111010) (dw nm)))

    ((ld ixl n)        (db #xdd #b00100110 n))
    ((ld ixh n)        (db #xdd #b00101110 n))
    ((ld (+ ix d) n)   (db #xdd #b00110110 d n))

    ((ld iyl n)        (db #xfd #b00100110 n))
    ((ld iyh n)        (db #xfd #b00101110 n))
    ((ld (+ iy d) n)   (db #xfd #b00110110 d n))

    ((ld bc nm)        (begin (db #b00000001) (dw nm)))
    ((ld de nm)        (begin (db #b00010001) (dw nm)))
    ((ld hl nm)        (begin (db #b00100001) (dw nm)))
    ((ld sp nm)        (begin (db #b00110001) (dw nm)))

    ((ld bc (nm))      (begin (db #xed #b01001011) (dw nm)))
    ((ld de (nm))      (begin (db #xed #b01011011) (dw nm)))
    ((ld hl (nm))      (begin (db      #b00101010) (dw nm)))
    ((ld ix (nm))      (begin (db #xdd #b00101010) (dw nm)))
    ((ld iy (nm))      (begin (db #xdd #b00101010) (dw nm)))
    ((ld sp (nm))      (begin (db #xed #b01111011) (dw nm)))

    ((ld (nm) bc)      (begin (db #xed #b01000011) (dw nm)))
    ((ld (nm) de)      (begin (db #xed #b01010011) (dw nm)))
    ((ld (nm) hl)      (begin (db      #b00100010) (dw nm)))
    ((ld (nm) ix)      (begin (db #xdd #b00100010) (dw nm)))
    ((ld (nm) iy)      (begin (db #xdd #b00100010) (dw nm)))
    ((ld (nm) sp)      (begin (db #xed #b01110011) (dw nm)))

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
    ((add (+ ix d))     (db #xdd #b10000110 d))

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

    ; Output
    ((out (n) a)       (db #xd3 n))

    ; Jump
    ((jp (hl))         (db #xe9))
    ((jp (ix))         (db #xdd #xe9))
    ((jp (iy))         (db #xfd #xe9))

    ; Jump (argument)
    ((jp nz nm)        (begin (db #b11000010) (dw nm)))
    ((jp z nm)         (begin (db #b11001010) (dw nm)))
    ((jp nc nm)        (begin (db #b11010010) (dw nm)))
    ((jp c nm)         (begin (db #b11011010) (dw nm)))
    ((jp po nm)        (begin (db #b11100010) (dw nm)))
    ((jp pe nm)        (begin (db #b11101010) (dw nm)))
    ((jp p nm)         (begin (db #b11110010) (dw nm)))
    ((jp m nm)         (begin (db #b11111010) (dw nm)))
    ((jp nm)           (begin (db #xc3) (dw nm)))

    ; Call and return
    ((call nm)         (begin (db #xcd) (dw nm)))

    ((call nz nm)      (begin (db #b11000100) (dw nm)))
    ((call z nm)       (begin (db #b11001100) (dw nm)))
    ((call nc nm)      (begin (db #b11010100) (dw nm)))
    ((call c nm)       (begin (db #b11011100) (dw nm)))
    ((call po nm)      (begin (db #b11100100) (dw nm)))
    ((call pe nm)      (begin (db #b11101100) (dw nm)))
    ((call p nm)       (begin (db #b11110100) (dw nm)))
    ((call m nm)       (begin (db #b11111100) (dw nm)))

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
  )

  (define-case-syntax (djnz e)
    (lets
      ($tmp (generate-temporary #'loop))
      #`(begin
        (db #x10)
        (db (u8 (- e #,$tmp)))
        (label #,$tmp))))

  (define-case-syntax (loop-djnz body ...)
    (lets
      ($tmp (generate-temporary #'loop-djnz))
      #`(begin
        (label #,$tmp)
        body ...
        (djnz #,$tmp))))

  (define-case-syntax (loop-jp body ...)
    (lets
      ($tmp (generate-temporary #'loop))
      #`(begin
        (label #,$tmp)
        body ...
        (jp #,$tmp))))

  (define-case-syntax (dup n body ...)
    #`(begin
      #,@(apply append
        (map-with (_ (iota (datum n)))
          #'(body ...)))))

  (define-syntax (run $syntax $lookup)
    (syntax-case $syntax ()
      ((_)
        #`(lets
          ($path "/tmp/main.nex")
          (%run
            (call-with-port (open-file-output-port $path (file-options no-fail))
              (lambda ($port)
                (put-blob $port
                  (nex-blob
                    (lets
                      ($blob (main-blob))
                      (%run (pretty-print (blob->bytevector $blob)))
                      $blob)))))
            (cspect $path))))))
)
