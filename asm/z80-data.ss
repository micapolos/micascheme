(library (asm z80-data)
  (export
    a f b c d e h l
    af bc de hl
    ix iy
    ixh ixl iyh iyl
    pc sp
    i r

    nz z nc po pe p m

    ld
    add adc sub sbc and or xor cp
    halt

    +)

  (import
    (rename (asm base)
      (and %and)
      (or %or)
      (xor %xor)
      (data %data)
      (+ %+))
    (asm data)
    (u))

  (define-keywords
    a f b c d e h l
    af bc de hl
    ix iy
    ixh ixl iyh iyl
    pc sp
    i r
    ehl dehl lde hlde
    nz z nc po pe p m
    +)

  (define-rules-syntaxes
    ((n? x)
      (u8? (datum x)))
    ((nn? x)
      (u16? (datum x)))
    ((db-233 a b c)
      (db (fxior (fxsll a 6) (fxsll b 3) c)))
    ((r? x)
      (syntax-case? #'x (b c d e h l hl a + ix iy)
        (b #t)
        (c #t)
        (d #t)
        (e #t)
        (h #t)
        (l #t)
        ((hl) #t)
        ((+ ix _) #t)
        ((+ iy _) #t)
        (a #t)))
    ((r-3 x)
      (syntax-case #'x (b c d e h l hl a + ix iy)
        (b #b000)
        (c #b001)
        (d #b010)
        (e #b011)
        (h #b100)
        (l #b101)
        ((hl) #b101)
        ((+ ix _) #b101)
        ((+ iy _) #b101)
        (a #b111)))
    ((r-prefix-data x)
      (syntax-case #'x (b c d e h l hl a + ix iy)
        (b (data))
        (c (data))
        (d (data))
        (e (data))
        (h (data))
        (l (data))
        ((hl) (data))
        ((+ ix _) (db #xdd))
        ((+ iy _) (db #xfd))
        (a (data))))
    ((r-offset-data x)
      (syntax-case #'x (b c d e h l hl a + ix iy)
        (b (data))
        (c (data))
        (d (data))
        (e (data))
        (h (data))
        (l (data))
        ((hl) (data))
        ((+ ix n) (db-d n))
        ((+ iy n) (db-d n))
        (a (data))))
    ((ihl? x)
      (syntax-case? #'x (hl)
        ((hl) #t)))
    ((d? x)
      (s8? (datum x)))
    ((db-d x)
      (db (fxand #xff (s8 (datum x))))))

  (define-rules-syntaxes
    ((alu op r)
      (r? r)
      (data
        (r-prefix-data r)
        (db-233 #b10 op (r-3 r))
        (r-offset-data r)))
    ((alu op n)
      (n? n)
      (data (db-233 #b11 op #b110) (db n)))
    ((ld r1 r2)
      (%and (r? r1) (r? r2) (not (%and (ihl? r1) (ihl? r2))))
      (db-233 #b01 (r-3 r1) (r-3 r2)))
    ((ld r n)
      (%and (r? r) (n? n))
      (data
        (db-233 #b00 (r-3 r) #b110)
        (db n)))
    ((add x) (alu #b000 x))
    ((adc x) (alu #b001 x))
    ((sub x) (alu #b010 x))
    ((sbc x) (alu #b011 x))
    ((and x) (alu #b100 x))
    ((or x)  (alu #b101 x))
    ((xor x) (alu #b110 x))
    ((cp x)  (alu #b111 x))
    ((halt) (db #b10110110)))
)
