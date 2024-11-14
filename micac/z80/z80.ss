(library (micac z80 z80)
  (export z80)
  (import
    (micac)
    (micac std))

  (micac
    (macro
      (z80
        posedge
        negedge
        address
        data
        m1?
        memory-request?
        read?
        write?
        wait?)
      (var uint16_t address 0)
      (var uint8_t data 0)
      (var bool memory-request?)
      (var bool read?)
      (var bool write?)
      (var bool m1?)
      (var bool m1-request? #t)
      (var bool fetch-cycle? #t)
      (var bool fetch-cycle-request? #t)
      (var bool write-cycle? #f)
      (var bool write-cycle-request? #t)
      (var uint8_t clock-cycle 0)
      (var uint8_t clock-cycle-request 0)
      (var uint16_t read-write-address 0)
      (var uint16_t pc 0)
      (var uint8_t r 0)
      (var bool wait? #f)
      (macro (posedge)
        (set clock-cycle clock-cycle-request)
        (set fetch-cycle? fetch-cycle-request?)
        (set write-cycle? write-cycle-request?)
        (set m1? m1-request?)

        (if fetch-cycle?
          (then
            (when (= clock-cycle 0)
              (set address pc)
              (inc pc))
            (when (= clock-cycle 2)
              (set address (bitwise-and r #x7f))
              (inc r)))
          (else
            (set address read-write-address))))
      (macro (negedge)
        (const uint8_t last-clock-cycle (if fetch-cycle? 3 2))
        (const bool last-clock-cycle? (= clock-cycle last-clock-cycle))
        (set clock-cycle-request (if last-clock-cycle? 0 (+ clock-cycle 1)))
        (set memory-request? (not last-clock-cycle?))
        (set m1-request?
          (and
            fetch-cycle-request?
            (zero? (bitwise-and clock-cycle #b10)))))))
)
