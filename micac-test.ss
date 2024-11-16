(import (micac) (only (micascheme) check equal?))

(check (equal? (micac (run (return 128))) 128))
