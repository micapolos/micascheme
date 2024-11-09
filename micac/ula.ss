(library (micac ula)
  (export ula)
  (import
    (micac)
    (micac std))

  (micac
    (macro (ula video-x video-y mem screen? red green blue update)
      (const int ula-width 256)
      (const int ula-height 192)
      (const int border 48)

      (var uint8_t bits)
      (var uint8_t attr)

      (var bool screen? #f)
      (var uint8_t red 0)
      (var uint8_t green 0)
      (var uint8_t blue 0)

      (macro (update)
        (set screen?
          (and
            (in-range? video-x border (+ border ula-width))
            (in-range? video-y border (+ border ula-height))))

        (when screen?
          (const int ula-x (- video-x border))
          (const int ula-y (- video-y border))
          (const bool read? (zero? (bitwise-and ula-x #x07)))
          (when read?
            (const int addr-x (bitwise-and (>> ula-x 3) #x1f))

            (const int bits-addr
              (bitwise-ior addr-x
                (<<
                  (bitwise-ior
                    (bitwise-and ula-y #xc0)
                    (<< (bitwise-and ula-y #x07) 3)
                    (>> (bitwise-and ula-y #x38) 3))
                  5)))

            (const int load-addr (<< frame-counter 1))
            (const bool bits? (> (>> bits-addr 3) load-addr))
            (set bits (? bits? #xff (ref scr (bits-addr))))

            (const int attr-addr (bitwise-ior #x1800 addr-x (<< (>> ula-y 3) 5)))
            (const bool attr? (> (>> attr-addr 3) load-addr))
            (set attr (? attr? #x07 (ref scr (attr-addr)))))

          (const bool pixel-on? (not (zero? (bitwise-and bits #x80))))
          (set bits (<< bits 1))

          (const bool flash-on? (not (zero? (bitwise-and attr #x80))))
          (const bool alternate-on? (not (zero? (bitwise-and frame-counter #x10))))
          (const bool ink-on? (? (and flash-on? alternate-on?) (not pixel-on?) pixel-on?))
          (const bool red? (not (zero? (bitwise-and attr (? ink-on? #x02 #x10)))))
          (const bool green? (not (zero? (bitwise-and attr (? ink-on? #x04 #x20)))))
          (const bool blue? (not (zero? (bitwise-and attr (? ink-on? #x01 #x08)))))
          (const bool bright? (not (zero? (bitwise-and attr #x40))))
          (const uint8_t color (? bright? #xFF #xBB))

          (set red (? red? color 0))
          (set green (? green? color 0))
          (set blue (? blue? color 0)))))))
