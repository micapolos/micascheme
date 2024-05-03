(library (emu dispatch)
  (export define-dispatch)
  (import (scheme) (syntax) (procedure))

  (define-rule-syntax (define-dispatch $id $op $body ...)
    (begin
      (define $immutable-vector
        (run
          (define $vector
            (make-vector 256 (lambda () (void))))
          (define-rule-syntax ($op $idx $expr)
            (vector-set! $vector $idx (lambda () $expr)))
          $body ...
          (vector->immutable-vector $vector)))
      (define-rule-syntax ($id $u8)
        (app (vector-ref $immutable-vector $u8)))))
)
