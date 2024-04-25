(library (masm model)
  (export
    i8? i8
    i16? i16
    int? int int-switch

    arrow? arrow arrow-ins arrow-outs
    type? type type-switch

    const? const const-int const-value
    inc? inc inc-int
    add? add add-int
    get? get get-int get-idx
    set? set set-int set-idx
    load? load load-int
    store? store store-int
    call? call call-idx
    out? out out-int

    op? op op-switch

    func? func func-arrow func-locals func-ops

    module? module module-funcs)
  (import (except (micascheme) load module))

  (data (i8))
  (data (i16))
  (enum (int i8 i16))

  (data (arrow ins outs))
  (enum (type int arrow))

  (data (const int value))
  (data (inc int))
  (data (add int))
  (data (get int idx))
  (data (set int idx))
  (data (load int))
  (data (store int))
  (data (call idx))
  (data (out int))

  (enum (op const inc add get set load store call out))

  (data (func arrow locals ops))

  (data (module funcs))
)
