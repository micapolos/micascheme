(library (asm z80n)
  (export
    label db dw

    a f b c d e h l
    af bc de hl
    ix iy
    ixh ixl iyh iyl
    pc sp
    i r

    ld
    di ei
    out jp loop halt

    run)
  (import
    (rename (micascheme) (run %run))
    (asm core)
    (nex)
    (cspect)
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
    i r)

  (define-rules-syntaxes
    (literals
      a f b c d e h l +
      af bc de hl
      ix iy
      ixh ixl iyh iyl
      pc sp
      i r)
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

    ((ld b ixh)       (db #xdd #b01000100))
    ((ld b ixl)       (db #xdd #b01000101))
    ((ld c ixh)       (db #xdd #b01001100))
    ((ld c ixl)       (db #xdd #b01001101))
    ((ld d ixh)       (db #xdd #b01010100))
    ((ld d ixl)       (db #xdd #b01010101))
    ((ld e ixh)       (db #xdd #b01011100))
    ((ld e ixl)       (db #xdd #b01011101))
    ((ld ixh ixh)     (db #xdd #b01100100))
    ((ld ixh ixl)     (db #xdd #b01100101))
    ((ld ixl ixh)     (db #xdd #b01101100))
    ((ld ixl ixl)     (db #xdd #b01101101))
    ((ld a ixh)       (db #xdd #b01111100))
    ((ld a ixl)       (db #xdd #b01111101))

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

    ((out (n) a) (db #xd3 n))
    ((jp nm) (begin (db #xc3) (dw nm))))

  (define-case-syntax (loop body ...)
    (lets
      ($tmp (generate-temporary #'loop))
      #`(begin
        (label #,$tmp)
        body ...
        (jp #,$tmp))))

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
