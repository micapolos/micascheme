(library (zexy ops)
  (export
    ; regs
    a f b c d e h l
    af af2 bc de hl
    ix iy ixh ixl iyh iyl
    pc sp i r

    ; flags
    z nz c nc po pe p m

    ; data
    db dw dz ds

    ; ops
    ld
    add adc sub sbc and xor or cp
    inc dec
    daa cpl neg ccf scf nop halt di ei im
    ex exx
    bit set res
    rlc rrc rl rr sla sra sli srl sll
    rla rlca rra rrca rld rrd
    jp djnz
    push pop
    call ret reti retn rst
    cpd cpdr cpi cpir ldd lddr ldi ldir
    in ind indr ini inir
    out outi otir outd otdr

    ; linker
    align label)

  (import
    (only (micascheme)
      define-syntax-rule
      define-aux-keyword
      begin ...))

  (define-syntax-rule (define-ops $op ...)
    (begin
      (define-aux-keyword $op) ...))

  (define-ops
    ; regs
    a f b c d e h l
    af af2 bc de hl
    ix iy ixh ixl iyh iyl
    pc sp i r

    ; flags
    z nz nc po pe p m

    ; data
    db dw dz ds

    ; ops
    ld
    add adc sub sbc and xor or cp
    inc dec
    daa cpl neg ccf scf nop halt di ei im
    ex exx
    bit set res
    rlc rrc rl rr sla sra sli srl sll
    rla rlca rra rrca rld rrd
    jp djnz
    push pop
    call ret reti retn rst
    cpd cpdr cpi cpir ldd lddr ldi ldir
    in ind indr ini inir
    out outi otir outd otdr

    ; linker
    align label)
)
