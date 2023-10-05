(import (micascheme) (z80))

(writeln
  (z80
    (call $proc)
    $proc
    (ret)))
