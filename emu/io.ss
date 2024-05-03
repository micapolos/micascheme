(library (emu io)
  (export null-io print-io)
  (import (scheme) (syntaxes) (procedure))

  (define-rules-syntaxes
    ((null-io _) 0)
    ((null-io _ _) (void))
    ((print-io $addr) (run (pretty-print `(io ,$addr)) 0))
    ((print-io $addr $val) (pretty-print `(io ,$addr ,$val))))
)
