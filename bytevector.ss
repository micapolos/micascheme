(library (bytevector)
  (export
    u16-bytevector
    u32-bytevector
    u64-bytevector)
  (import (scheme) (lets))

  (define (u16-bytevector $u16 $eness)
    (lets
      ($bytevector (make-bytevector 2))
      (run (bytevector-u16-set! $bytevector 0 $u16 $eness))
      (bytevector->immutable-bytevector $bytevector)))

  (define (u32-bytevector $u32 $eness)
    (lets
      ($bytevector (make-bytevector 4))
      (run (bytevector-u32-set! $bytevector 0 $u32 $eness))
      (bytevector->immutable-bytevector $bytevector)))

  (define (u64-bytevector $u64 $eness)
    (lets
      ($bytevector (make-bytevector 8))
      (run (bytevector-u64-set! $bytevector 0 $u64 $eness))
      (bytevector->immutable-bytevector $bytevector)))
)
