(library (micac ast)
  (export
    u8 u8?
    u16 u16?
    u32 u32?
    type type? type-switch

    const const? const-value
    variable variable? variable-index
    value value? value-switch

    alloc alloc? alloc-type
    block block? block-instrs
    ld ld? ld-size ld-variable ld-value
    add add? add-size add-variable add-value
    instr instr? instr-switch)
  (import (micascheme))

  (data (u8))
  (data (u16))
  (data (u32))
  (enum (type u8 u16 u32))

  (data (const value))
  (data (variable index))
  (enum (value const variable))

  (data (alloc type))
  (data (ld size variable value))
  (data (add size variable value))
  (data (block instrs))
  (enum (instr alloc ld block))
)
