(library (zx-next scheme alloc)
  (export
    alloc-header-gc-mark-mask
    alloc-header-values-mask
    alloc-header-pot-size-mask

    alloc-header-symbol-tag
    alloc-header-string-tag
    alloc-header-pair-tag

    symbol-data
    string-data)
  (import (zx-next core))

  (define-values
    ; 0 = non marked, 1 = marked
    (alloc-header-gc-mark-mask    #b10000000)

    ; 0 = byte vector, 1 = value vector
    (alloc-header-values-mask     #b01000000)

    ; type
    (alloc-header-type-mask       #b00110000)

    ; allocation size as power-of-two
    (alloc-header-pot-size-mask   #b00001111))

  (define-values
    (alloc-header-symbol-tag        #b000)
    (alloc-header-string-tag        #b001)
    (alloc-header-pair-tag          #b100))

  (define-ops
    ((symbol-data s)
      (align 4)
      (db #b00000000)
      (dz s))
    ((string-data s)
      (align 4)
      (db #b00010000)
      (dz s)))
)
