(library (masm model)
  (export
    const-op? const-op const-op-u8
    inc-op? inc-op
    add-op? add-op
    get-op? get-op get-op-id
    set-op? set-op set-op-id
    load-op? load-op
    store-op? store-op
    out-op? out-op)
  (import (micascheme))

  (data (const-op u8))
  (data (inc-op))
  (data (add-op))
  (data (get-op id))
  (data (set-op id))
  (data (load-op))
  (data (store-op))
  (data (out-op))
)
