(import (micascheme) (leo load) (only (leo scheme)))

(for-each load-leo (command-line-arguments))
