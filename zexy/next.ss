(library (zexy next)
  (export
    make-next next? next-mmus next-banks)
  (import
    (zexy base))

  (define-record next () (
    ((immutable mmus) (make-bytevector 8))
    ((immutable banks) (build-immutable-vector #x100 make-bank))))

  (define (make-bank $index)
    (make-bytevector #x2000))
)
