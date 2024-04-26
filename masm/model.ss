(library (masm model)
  (export
    i8? i8
    i16? i16
    int? int int-switch

    arrow? arrow arrow-ins arrow-outs
    type? type type-switch

    drop? drop
    select? select

    const? const const-int const-value
    inc? inc inc-int
    add? add add-int
    local-get? local-get local-get-type local-get-idx
    local-set? local-set local-set-type local-set-idx
    mem-get? mem-get mem-get-int
    mem-set? mem-set mem-set-int
    io-get? io-get
    io-set? io-set
    call? call call-idx


    nop? nop
    trap? trap
    block? block block-ops

    op? op op-switch

    func? func func-arrow func-locals func-ops

    module? module module-funcs)
  (import (except (micascheme) module))

  (data (i8))
  (data (i16))
  (enum (int i8 i16))

  (data (arrow ins outs))
  (enum (type int arrow))

  (data (drop))
  (data (select))

  (data (const int value))
  (data (inc int))
  (data (add int))
  (data (local-get type idx))
  (data (local-set type idx))
  (data (mem-get int))
  (data (mem-set int))
  (data (io-get))
  (data (io-set))
  (data (call idx))

  (data (nop))
  (data (trap))
  (data (block ops))

  (enum
    (op
      drop select
      const inc add local-get local-set mem-get mem-set io-get io-set call
      nop trap block))

  (data (func arrow locals ops))

  (data (module funcs))
)
