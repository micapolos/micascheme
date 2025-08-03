(import (zx-next test))

(test
  (case case-1)
  (case case-2)
  ; Simulated failure
  (catch (case case-3 (fail)))
  (when z (fail)))
