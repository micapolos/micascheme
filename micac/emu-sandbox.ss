(import
  (micascheme)
  (micac c)
  (micac std)
  (micac run)
  (micac emu))

(run-emu
  (clock (* 352 288 60))
  (video 352 288)
  (init
    (const int bar-size 3365)
    (var uint8_t (* pixel-ref))
    (var int pixel-counter 0)
    (var int bar-counter 0)
    (var uint8_t red #xff)
    (var uint8_t green #xff)
    (var uint8_t blue 0))
  (update
    (on (= bar-counter 0)
      (set bar-counter bar-size)
      (set red (inv red))
      (set green (inv green))
      (set blue (inv blue)))
    (dec bar-counter)

    (on (= pixel-counter 0)
      (set pixel-ref pixels)
      (set pixel-counter pixel-count))
    (dec pixel-counter)

    ; alpha
    (set (pixel-ref *) #xff)
    (inc pixel-ref)

    ; red
    (set (pixel-ref *) red)
    (inc pixel-ref)

    ; green
    (set (pixel-ref *) green)
    (inc pixel-ref)

    ; blue
    (set (pixel-ref *) blue)
    (inc pixel-ref)))
