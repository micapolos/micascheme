(import (check) (binder) (lets))

(define-accessors (string-binder string->number string-length string->list))

(define-accessors (string-binder-vararg string->number string-length . string->list))
