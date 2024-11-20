(import (scheme) (check) (throw))

; === raises? ===

(check (not (raises? (lambda () 128))))
(check (raises? (lambda () (throw error))))

(check (raises (throw error)))
(check (works 123))
