(import (check) (syntax))

(define-aux-keyword foo)
(check (raises? (lambda () foo)))
