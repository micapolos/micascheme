(library (zx-next rgb)
  (export rgb-333)
  (import (zx-next core))

  (define-ops
    ((rgb-333 r g b)
      (db (fxior (fxsll r 5) (fxsll g 2) (fxsrl b 1)))
      (db (fxand b 1)))
    ((rgb-332 r g b)
      (db (fxior (fxsll r 5) (fxsll g 2) b))))
)
