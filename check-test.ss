(import (scheme) (check) (throw))

(check (raises (throw error)))
(check (works 123))
