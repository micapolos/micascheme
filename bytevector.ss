(library (bytevector)
  (export u16-bytevector)
  (import (scheme) (lets))

  (define (u16-bytevector $u16 $eness)
    (lets
      ($bytevector (make-bytevector 2))
      (run (bytevector-u16-set! $bytevector 0 $u16 $eness))
      (bytevector->immutable-bytevector $bytevector)))

)
