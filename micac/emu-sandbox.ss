(import
  (micac)
  (micac std)
  (micac emu)
  (micac sdl)
  (c run))

(c-run-echo? #t)

(run-emu
  (video 352 288 96 24 4) ; width height h-blank v-blank cycles-per-pixel
  (init
    (const int border 48)
    (const int ula-width 256)
    (const int ula-height 192)

    (const int bar-size 4630)
    (var int bar-counter 0)

    (var uint8_t background-red #xff)
    (var uint8_t background-green #xff)
    (var uint8_t background-blue 0)

    (var int frame-counter 0)

    (var uint8_t bits)
    (var uint8_t attr)

    (file scr scr-size "/Users/micapolos/git/micascheme/micac/scr/Cobra.scr"))
  (update
    (when (zero? pixel-cycle-counter)
      (const bool screen?
        (and
          (in-range? video-x border (+ border ula-width))
          (in-range? video-y border (+ border ula-height))))

      (if screen?
        (then
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

          (const bool ula?
            (or
              (and (>= video-x mouse-x) (>= video-y mouse-y))
              (and (< video-x mouse-x) (< video-y mouse-y))))
          (if (xor ula? mouse-pressed?)
            (then
              (set red (? red? color 0))
              (set green (? green? color 0))
              (set blue (? blue? color 0)))
            (else
              (set red (- frame-counter video-x))
              (set green (- frame-counter video-y))
              (set blue (+ frame-counter (>> (* video-x video-y) 6))))))
        (else
          (set red background-red)
          (set green background-green)
          (set blue background-blue)))

      (inc bar-counter)

      (when (= bar-counter bar-size)
        (set bar-counter 0)
        (set background-red (inv background-red))
        (set background-green (inv background-green))
        (set background-blue (inv background-blue)))

      (const bool frame-start?
        (and (= video-x 0) (= video-y 0)))

      (when frame-start? (inc frame-counter)))))
