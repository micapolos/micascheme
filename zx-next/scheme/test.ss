(library (zx-next scheme test)
  (export)
  (import
    (only (micascheme) export import)
    (zx-next test))
  (export
    (import
      (except (zx-next test) assert throw write if when)
      (zx-next scheme assert))))
