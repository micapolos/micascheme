(import (micascheme) (asm-3 fragment) (syntax lookup))

(check
  (equal?
    (fragment->bytevector 100 (lookup-with) (db 10 20 30))
    (bytevector 10 20 30)))

; (check
;   (equal?
;     (fragment->bytevector 100 (lookup-with) (db org))
;     (bytevector 10 20 30)))
