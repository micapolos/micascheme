(import (sjasm lang))

(sjasm
  (device zxspectrumnext)
  (org #x8000)
  (Main : (jr Main))
  (savenex open "/tmp/main.nex" Main #xbfe0)
  (savenex auto)
  (savenex close)
  (cspectmap "/tmp/main.map"))
