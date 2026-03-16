(import (micascheme) (leo load) (prefix (leo scheme) %))

; Trigger anything from (leo scheme), so it's included
(%any? #t)

(for-each load-leo (command-line-arguments))
