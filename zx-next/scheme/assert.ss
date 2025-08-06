(library (zx-next scheme assert)
  (export value assert)
  (import
    (zx-next core)
    (zx-next scheme prims)
    (prefix (zx-next assert) zx-))

  (define-ops (keywords value)
    ((assert value v)
      (assert de (fxsrl value 16))
      (assert hl (fxand value #xffff)))
    ((assert . x)
      (zx-assert . x)))
)
