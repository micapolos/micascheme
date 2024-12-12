(library (asm z80-keywords)
  (export
    db dw +

    nop ld halt

    add adc sub sbc
    and or xor cp
    inc dec

    pc sp
    a f b c d e h l
    af bc de hl
    ixh ixl iyh iyl
    ix iy

    i r)
  (import (only (micascheme) define-aux-keywords))

  (define-aux-keywords
    db dw +

    nop ld halt

    add adc sub sbc
    and or xor cp
    inc dec

    pc sp
    a f b c d e h l
    af bc de hl
    ixh ixl iyh iyl
    ix iy

    i r)
)
