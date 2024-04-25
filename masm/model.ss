(library (masm model)
  (export
    u8? u8
    u16? u16

    type? type type-switch

    const? const const-type const-value
    inc? inc inc-type
    add? add add-type
    get? get get-type get-idx
    set? set set-type set-idx
    load? load load-type
    store? store store-type
    out? out out-type

    op? op op-switch)
  (import (except (micascheme) load))

  (data (u8))
  (data (u16))

  (enum (type u8 u16))

  (data (const type value))
  (data (inc type))
  (data (add type))
  (data (get type idx))
  (data (set type idx))
  (data (load type))
  (data (store type))
  (data (out type))

  (enum (op const inc add get set load store out))
)
