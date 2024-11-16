(import (micascheme) (micac run))

(check (equal? (micac-run) 0))
(check (equal? (micac-run (return 128)) 128))
