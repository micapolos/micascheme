(import (check) (binder) (lets))

(define-binder (string-binder string->number string-length string->list))

(define-binder (string-binder-vararg string->number string-length . string->list))
