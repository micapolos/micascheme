(import
  (scheme)
  (emu mem)
  (emu z80)
  (emu run))

(define-mem mem #x10000)
(define-z80 mem z80-step z80-dump)

(do
  (($i 0 (add1 $i)))
  ((= $i #x10000) (void))
  (mem $i (random #x100)))

(time (run z80-step 35000000))

(z80-dump)
