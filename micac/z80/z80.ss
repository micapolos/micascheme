(library (micac z80 z80)
  (export z80)
  (import
    (micac)
    (micac std))

  (micac
    (macro
      (z80
        z80-posedge
        z80-negedge
        z80-address
        z80-data
        z80-m1?
        z80-memory-request?
        z80-read?
        z80-write?
        z80-wait?)
      (var uint16_t z80-address 0)
      (var uint8_t z80-data 0)
      (var bool z80-memory-request?)
      (var bool z80-read?)
      (var bool z80-write?)
      (var bool z80-m1?)
      (var bool z80-m1-request? #t)
      (var bool z80-fetch-cycle? #t)
      (var bool z80-fetch-cycle-request? #t)
      (var bool z80-write-cycle? #f)
      (var bool z80-write-cycle-request? #t)
      (var uint8_t z80-clock-cycle 0)
      (var uint8_t z80-clock-cycle-request 0)
      (var uint16_t z80-read-write-address 0)
      (var uint16_t z80-pc 0)
      (var uint8_t z80-r 0)
      (var bool z80-wait? #f)
      (macro (z80-posedge)
        (set z80-clock-cycle z80-clock-cycle-request)
        (set z80-fetch-cycle? z80-fetch-cycle-request?)
        (set z80-write-cycle? z80-write-cycle-request?)
        (set z80-m1? z80-m1-request?)

        (if z80-fetch-cycle?
          (then
            (when (= z80-clock-cycle 0)
              (set z80-address z80-pc)
              (inc z80-pc))
            (when (= z80-clock-cycle 2)
              (set z80-address (bitwise-and z80-r #x7f))
              (inc z80-r)))
          (else
            (set z80-address z80-read-write-address))))
      (macro (z80-negedge)
        (const uint8_t z80-last-clock-cycle (? z80-fetch-cycle? 3 2))
        (const bool z80-last-clock-cycle? (= z80-clock-cycle z80-last-clock-cycle))
        (set z80-clock-cycle-request (? z80-last-clock-cycle? 0 (+ z80-clock-cycle 1)))
        (set z80-memory-request? (not z80-last-clock-cycle?))
        (set z80-m1-request?
          (and
            z80-fetch-cycle-request?
            (zero? (bitwise-and z80-clock-cycle #b10)))))))
)
