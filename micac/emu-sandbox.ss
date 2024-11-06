(import
  (micac)
  (micac std)
  (micac emu)
  (micac sdl))

(run-emu
  (video 352 288 96 24 4) ; width height h-blank v-blank cycles-per-pixel
  (init
    (const bool ula? #t)

    (const int border 48)
    (const int h-screen 256)
    (const int v-screen 192)

    (const int bar-size 4630)
    (var int bar-counter 0)

    (var uint8_t bg-red #xff)
    (var uint8_t bg-green #xff)
    (var uint8_t bg-blue 0)

    (var int frame-counter 0)

    (var uint8_t bits)
    (var uint8_t attr)

    (sdl-file-data data data-size "/Users/micapolos/git/micascheme/micac/scr/Spellbound.scr"))
  (update
    (when (zero? pixel-cycle-counter)
      (const bool screen?
        (and
          (in-range? h-counter border (+ border h-screen))
          (in-range? v-counter border (+ border v-screen))))

      (if screen?
        (then
          (const int x (- h-counter border))
          (const int y (- v-counter border))
          (const bool read? (zero? (bitwise-and x #x07)))
          (when read?
            (const int h-addr (bitwise-and (>> x 3) #x1f))

            (const int bits-addr
              (bitwise-ior h-addr
                (<<
                  (bitwise-ior
                    (bitwise-and y #xc0)
                    (<< (bitwise-and y #x07) 3)
                    (>> (bitwise-and y #x38) 3))
                  5)))

            (const int load-addr (<< frame-counter 1))
            (const bool bits? (> (>> bits-addr 3) load-addr))
            (set bits (? bits? #xff (ref data (bits-addr))))

            (const int attr-addr (bitwise-ior #x1800 h-addr (<< (>> y 3) 5)))
            (const bool attr? (> (>> attr-addr 3) load-addr))
            (set attr (? attr? #x07 (ref data (attr-addr)))))

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

          (if ula?
            (then
              (set red (? red? color 0))
              (set green (? green? color 0))
              (set blue (? blue? color 0)))
            (else
              (set red (- frame-counter h-counter))
              (set green (- frame-counter v-counter))
              (set blue (+ frame-counter (bitwise-arithmetic-shift-right (* h-counter v-counter) 6))))))
        (else
          (set red bg-red)
          (set green bg-green)
          (set blue bg-blue)))

      (inc bar-counter)

      (when (= bar-counter bar-size)
        (set bar-counter 0)
        (set bg-red (inv bg-red))
        (set bg-green (inv bg-green))
        (set bg-blue (inv bg-blue)))

      (const bool frame-start?
        (and (= h-counter 0) (= v-counter 0)))

      (when frame-start? (inc frame-counter)))))
