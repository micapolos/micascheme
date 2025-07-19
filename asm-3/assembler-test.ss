(import (micascheme) (asm-3 assembler) (asm-3 expression) (asm-3 fragment) (asm-3 located) (syntax lookup))

(define dependent-lookup
  (lookup-with
    (val-20 (syntax->expression #'20))
    (sub-30 (db 30))
    (sub-40 (db 40))
    (call-30-40 (db 255 sub-30 255 sub-40))
    (main (db 10 val-20 255 call-30-40 255 sub-40 255 sub-30 org))))

(check
  (equal?
    (assemble dependent-lookup 100 #'sub-30)
    (located 100 (bytevector 30))))

(check
  (equal?
    (assemble dependent-lookup 100 #'sub-40)
    (located 100 (bytevector 40))))

(check
  (equal?
    (assemble dependent-lookup 100 #'call-30-40)
    (located 102 (bytevector 30 40 #xff 100 #xff 101))))

(check
  (equal?
    (assemble dependent-lookup 100 #'main)
    (located 106 (bytevector 30 40 255 100 255 101 10 20 255 102 255 101 255 100 106))))
