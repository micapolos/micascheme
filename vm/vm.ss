(library (vm vm)
  (export
    make-vm
    make-empty-vm
    vm-bytevector
    vm-sp
    vm-sp-set!)
  (import (micascheme))

  (define-record-type vm
    (fields
      (immutable bytevector)
      (mutable sp)))

  (define (make-empty-vm $size)
    (make-vm (make-bytevector $size 0) $size))

  (record-type-equal-procedure
    (record-type-descriptor vm)
    (lambda ($vm-1 $vm-2 $eq?)
      (and
        (bytevector=? (vm-bytevector $vm-1) (vm-bytevector $vm-2))
        (= (vm-sp $vm-1) (vm-sp $vm-2)))))

  (record-type-hash-procedure
    (record-type-descriptor vm)
    (lambda ($vm $hash)
      (bitwise-xor
        ($hash (vm-bytevector $vm))
        ($hash (vm-sp $vm)))))
)
