(import
  (micac)
  (micac std)
  (micac emu))

(run-emu
  (clock (* 448 312 60 4)) ; h-count v-count fps cycles-per-pixel
  (video 352 288 96 24 4) ; width height h-blank v-blank cycles-per-pixel
  (init
    (const int border 48)
    (const int h-screen 256)
    (const int v-screen 192)

    (const int bar-size 4630)
    (var int bar-counter 0)

    (var uint8_t bg-red #xff)
    (var uint8_t bg-green #xff)
    (var uint8_t bg-blue 0)

    (var int frame-counter 0))
  (update
    (when (zero? pixel-cycle-counter)
      (const bool screen?
        (and
          (in-range? h-counter border (+ border h-screen))
          (in-range? v-counter border (+ border v-screen))))

      (if screen?
        (then
          (set red (- frame-counter h-counter))
          (set green (- frame-counter v-counter))
          (set blue (+ frame-counter (bitwise-arithmetic-shift-right (* h-counter v-counter) 6))))
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
