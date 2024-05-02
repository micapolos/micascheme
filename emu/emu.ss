(import
  (scheme)
  (emu mem)
  (emu z80))

(define-mem mem #x10000)
(define-z80 mem z80-step z80-dump)

(do
  (($i 0 (add1 $i)))
  ((= $i #x10000) (void))
  (mem $i (random #x100)))

(time
  (do
    (($i 35000000 (fx-/wraparound $i 1)))
    ((fxzero? $i) (void))
    (z80-step)))

(z80-dump)
