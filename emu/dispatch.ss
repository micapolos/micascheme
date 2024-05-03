(library (emu dispatch)
  (export define-dispatch define-dispatch-8)
  (import (scheme) (syntax) (procedure) (emu internal))

  (define-internal dispatch-vector)

  (define-rule-syntax (define-dispatch $id $item ...)
    (begin
      (define-rule-syntax ($id $index)
        (app (vector-ref (dispatch-vector $id) $index)))
      (define-dispatch-vector $id
        (immutable-vector (lambda () $item) ...))))

  (define-rule-syntax (define-dispatch-8 ($id $set) $body ...)
    (begin
      (define $immutable-vector
        (run
          (define $vector
            (make-vector 256 (lambda () (void))))
          (define-rule-syntax ($set $idx $expr)
            (vector-set! $vector $idx (lambda () $expr)))
          $body ...
          (vector->immutable-vector $vector)))
      (define-rule-syntax ($id $u8)
        (app (vector-ref $immutable-vector $u8)))))
)
