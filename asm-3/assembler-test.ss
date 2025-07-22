(import
  (asm-3 base)
  (asm-3 assembler)
  (asm-3 expression)
  (asm-3 syntax-expression)
  (asm-3 fragment)
  (asm-3 assembled)
  (syntax lookup))

(define dependent-lookup
  (lookup-with
    (+ (pure-expression +))
    (val-20 (pure-expression 20))
    (val-30 (syntax->expression #'(+ 10 val-20)))
    (data-10 (db 10))
    (data-20 (db val-20))
    (data-30 (db val-30))
    (data-10-20-30 (db 10 val-20 val-30))
    (data-org (db org))
    (data-org+10 (db (+ org 10)))
    (data-org+20 (db (+ org val-20)))
    (data-org+30 (db (+ org val-30)))
    (sub-20 (db val-20))
    (sub-30 (db 30))
    (sub-40 (db 40))
    (call-30-40 (db 255 sub-30 255 sub-40))
    (main (db 10 val-20 255 call-30-40 255 sub-40 255 sub-30 org))))

(check
  (equal?
    (assemble-identifier dependent-lookup 100 #'data-10)
    (assembled 100 (bytevector 10))))

(check
  (equal?
    (assemble-identifier dependent-lookup 100 #'data-20)
    (assembled 100 (bytevector 20))))

(check
  (equal?
    (assemble-identifier dependent-lookup 100 #'data-30)
    (assembled 100 (bytevector 30))))

(check
  (equal?
    (assemble-identifier dependent-lookup 100 #'data-10-20-30)
    (assembled 100 (bytevector 10 20 30))))

(check
  (equal?
    (assemble-identifier dependent-lookup 100 #'data-org)
    (assembled 100 (bytevector 100))))

(check
  (equal?
    (assemble-identifier dependent-lookup 100 #'data-org+10)
    (assembled 100 (bytevector 110))))

(check
  (equal?
    (assemble-identifier dependent-lookup 100 #'data-org+20)
    (assembled 100 (bytevector 120))))

(check
  (equal?
    (assemble-identifier dependent-lookup 100 #'data-org+30)
    (assembled 100 (bytevector 130))))

(check
  (equal?
    (assemble-identifier dependent-lookup 100 #'sub-30)
    (assembled 100 (bytevector 30))))

(check
  (equal?
    (assemble-identifier dependent-lookup 100 #'sub-40)
    (assembled 100 (bytevector 40))))

(check
  (equal?
    (assemble-identifier dependent-lookup 100 #'call-30-40)
    (assembled 102 (bytevector 30 40 #xff 100 #xff 101))))

(check
  (equal?
    (assemble-identifier dependent-lookup 100 #'main)
    (assembled 106 (bytevector 30 40 255 100 255 101 10 20 255 102 255 101 255 100 106))))
