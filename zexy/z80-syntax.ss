(library (zexy z80-syntax)
  (export
    ld
    add adc sub sbc and or xor cp
    inc dec
    in out
    call rst ret reti retn
    jp jr djnz
    ex exx
    ldd ldi lddr ldir
    outi outd otir otdr
    push pop

    a f b c d e h l
    ixl ixh iyl iyh
    af af2 bc de hl ix iy
    sp pc i r

    z nz nc po pe p m

    r-op
    rr-sp-op
    rr-af-op
    alu-op
    seq-op
    stack-op
    rot-op)
  (import
    (except (zexy base) and or xor push pop))

  (define-rule-syntax (define-keywords keyword ...)
    (begin
      (define-rule-syntax keyword
        (syntax-error 'keyword
          (string-append "misplaced asm keyword"))) ...))

  (define-keywords
    ; instructions
    ld
    add adc sub sbc and or xor cp
    inc dec
    in out
    call rst ret reti retn
    jp jr djnz
    ex exx
    ldd ldi lddr ldir
    outi outd otir otdr
    push pop

    ; registers
    a f b c d e h l
    ixl ixh iyl iyh
    af af2 bc de hl ix iy
    sp pc i r

    ; flags (without c)
    z nz nc po pe p m)

  (define (r-op $syntax)
    (syntax-case $syntax (b c d e h l hl a)
      (b #b000)
      (c #b001)
      (d #b010)
      (e #b011)
      (h #b100)
      (l #b101)
      ((hl) #b110)
      (a #b111)
      (else #f)))

  (define (rr-sp-op $syntax)
    (syntax-case $syntax (bc de hl sp)
      (bc #b00)
      (de #b01)
      (hl #b10)
      (sp #b11)
      (else #f)))

  (define (rr-af-op $syntax)
    (syntax-case $syntax (bc de hl af)
      (bc #b00)
      (de #b01)
      (hl #b10)
      (af #b11)
      (else #f)))

  (define (alu-op $syntax)
    (syntax-case $syntax (add adc sub sbc and xor or cp)
      (add #b000)
      (adc #b001)
      (sub #b010)
      (sbc #b011)
      (and #b100)
      (xor #b101)
      (or #b110)
      (cp #b111)
      (else #f)))

  (define (seq-op $syntax)
    (syntax-case $syntax (inc dec)
      (inc #b0)
      (dec #b1)
      (else #f)))

  (define (stack-op $syntax)
    (syntax-case $syntax (pop push)
      (pop #b0)
      (push #b1)
      (else #f)))

  (define (rot-op $syntax)
    (syntax-case $syntax (rlc rrc rl rr sla sra sli srl)
      (rlc #b000)
      (rrc #b000)
      (rl #b000)
      (rr #b000)
      (sla #b000)
      (sra #b000)
      (sli #b000)
      (srl #b000)
      (else #f)))
)
