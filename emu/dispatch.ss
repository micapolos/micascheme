(library (emu dispatch)
  (export define-dispatch)
  (import (scheme) (syntax) (lets))

  (define-rule-syntax (dispatch-vector $op $body ...)
    (let ()
      (define $vector (make-vector 256 (lambda () (void))))

      (define-rule-syntax ($op $idx $expr)
        (vector-set! $vector $idx (lambda () $expr)))

      $body ...

      (vector->immutable-vector $vector)))

  (define-rule-syntax (define-dispatch $id $op $body ...)
    (begin
      (define $vector (dispatch-vector $op $body ...))
      (define-rule-syntax ($id $u8) ((vector-ref $vector $u8)))))
)
