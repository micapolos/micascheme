(import (check))

; === raises? ===

(check (not (raises? (lambda () 128))))
(check (raises? (lambda () (throw error))))

