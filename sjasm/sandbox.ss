(import (micascheme) (sjasm run) (sjasm string) (sjasm keywords))

(sjasm-run
  (sjasm-string
    (device zxspectrumnext)
    (org #x8000)
    (Main : (jr Main))
    (savenex open "/tmp/main.nex" Main #xbfe0)
    (savenex auto)
    (savenex close)
    (cspectmap "/tmp/main.map")))
