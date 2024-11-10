(import
  (micac)
  (micac emu)
  (micac std)
  (micac z80 z80))

(run-emu
  (video 352 288 96 24 1)
  (var bool clk? #t)
  (z80 z80-posedge z80-negedge z80-address z80-data z80-m1? z80-memory-request? z80-read? z80-write? z80-wait?)
  (update
    (if clk?
      (then (z80-posedge))
      (else (z80-negedge)))

    (set clk? (not clk?))

    (set red (? z80-m1? #xff 0))
    (set green (bitwise-and z80-address #xff))
))
