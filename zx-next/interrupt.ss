(library (zx-next interrupt)
  (export with-di)
  (import (zx-next core))

  (define-ops
    ((with-di body ...)
      (di)
      body ...
      (ei)))
)
