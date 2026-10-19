(library (arm64 encode)
  (export
    load/store-pair)
  (import
    (scheme)
    (fixnum))

  (define (load/store-pair size1 op8 offset7 rt2 rn rt)
    (fxpack
      (1 size1)
      (1 0)
      (8 op8)
      (7 offset7)
      (5 rt2)
      (5 rn)
      (5 rt)))
)
