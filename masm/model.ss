(library (masm model)
  (export
    u8? u8
    u16? u16

    type? type type-switch

    const? const const-u8
    inc? inc
    add? add
    get? get get-idx
    set? set set-idx
    load? load
    store? store
    out? out

    op? op op-switch)
  (import (except (micascheme) load))

  (data (u8))
  (data (u16))

  (enum (type u8 u16))

  (data (const u8))
  (data (inc))
  (data (add))
  (data (get idx))
  (data (set idx))
  (data (load))
  (data (store))
  (data (out))

  (enum (op const inc add get set load store out))
)
