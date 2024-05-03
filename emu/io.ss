(library (emu io)
  (export null-io print-io char-io)
  (import (scheme) (syntaxes) (procedure) (emu math))

  (define-rules-syntaxes
    ((null-io _) 0)
    ((null-io _ _) (void))
    ((print-io $addr) (run (pretty-print `(io ,$addr)) 0))
    ((print-io $addr $val) (pretty-print `(io ,$addr ,$val)))
    ((char-io _) (fx->u8 (char->integer (read-char))))
    ((char-io _ $val) (write-char (integer->char $val))))
)
