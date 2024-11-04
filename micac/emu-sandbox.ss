(import
  (micac)
  (micac std)
  (micac emu))

(micac
  (run
    (emu
      (clock (* 352 288 60))
      (video 352 288)
      (init
        (const int bar-size 3365)
        (var uint8_t (* pixel-ref) pixels)
        (var int pixel-counter pixel-count)
        (var int bar-counter bar-size)
        (var uint8_t alpha #xff)
        (var uint8_t red #xff)
        (var uint8_t green #xff)
        (var uint8_t blue 0))
      (update
        (set (pixel-ref *) alpha)
        (inc pixel-ref)

        (set (pixel-ref *) red)
        (inc pixel-ref)

        (set (pixel-ref *) green)
        (inc pixel-ref)

        (set (pixel-ref *) blue)
        (inc pixel-ref)

        (dec bar-counter)
        (when (= bar-counter 0)
          (set bar-counter bar-size)
          (set red (inv red))
          (set green (inv green))
          (set blue (inv blue)))

        (dec pixel-counter)
        (when (= pixel-counter 0)
          (set pixel-ref pixels)
          (set pixel-counter pixel-count))))))
