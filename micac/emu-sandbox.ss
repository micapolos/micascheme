(import
  (micac)
  (micac std)
  (micac emu))

(run-emu
  (clock (* 448 312 60))
  (video 352 288)
  (init
    (const int border 48)
    (const int h-screen 256)
    (const int h-blank 96)
    (const int v-screen 192)
    (const int v-blank 24)
    (const int h-size (+ border h-screen border h-blank))
    (const int v-size (+ border v-screen border v-blank))

    (var uint8_t (* pixel-ref) pixels) ; initially this should be offset at (0, 0)

    (const int bar-size 4630)
    (var int bar-counter bar-size)

    (var uint8_t bg-red #xff)
    (var uint8_t bg-green #xff)
    (var uint8_t bg-blue 0)

    (var int v-counter 0)
    (var int h-counter 0)

    (var bool h-screen? #t)
    (var bool v-screen? #t)
    (var bool h-pixel? #t)
    (var bool v-pixel? #t))

  (update
    (begin
      (var uint8_t red)
      (var uint8_t green)
      (var uint8_t blue)

      (const bool screen? (and h-screen? v-screen?))
      (if screen?
        (begin
          (set red #xc0)
          (set green #xc0)
          (set blue #xc0))
        (begin
          (set red bg-red)
          (set green bg-green)
          (set blue bg-blue)))

      (when (and h-pixel? v-pixel?)
        (set (pixel-ref *) #xff) ; alpha
        (inc pixel-ref)

        (set (pixel-ref *) red)
        (inc pixel-ref)

        (set (pixel-ref *) green)
        (inc pixel-ref)

        (set (pixel-ref *) blue)
        (inc pixel-ref)))

    (dec bar-counter)
    (when (= bar-counter 0)
      (set bar-counter bar-size)
      (set bg-red (inv bg-red))
      (set bg-green (inv bg-green))
      (set bg-blue (inv bg-blue)))

    (inc h-counter)
    (when (= h-counter h-size)
      (set h-counter 0)
      (inc v-counter)
      (when (= v-counter v-size)
        (set v-counter 0)))

    (begin
      (const bool h-screen-start? (= h-counter 0))
      (const bool h-screen-end? (= h-counter h-screen))

      (const bool v-screen-start? (= v-counter 0))
      (const bool v-screen-end? (= v-counter v-screen))

      (const bool h-pixel-start? (= h-counter (+ h-screen border h-blank)))
      (const bool h-pixel-end? (= h-counter (+ h-screen border)))

      (const bool v-pixel-start? (= v-counter (+ v-screen border v-blank)))
      (const bool v-pixel-end? (= v-counter (+ v-screen border)))

      (const bool h-screen-flip? (or h-screen-start? h-screen-end?))
      (const bool v-screen-flip? (and h-screen-start? (or v-screen-start? v-screen-end?)))

      (const bool h-pixel-flip? (or h-pixel-start? h-pixel-end?))
      (const bool v-pixel-flip? (and h-pixel-start? (or v-pixel-start? v-pixel-end?)))

      (when h-screen-flip? (set h-screen? (not h-screen?)))
      (when v-screen-flip? (set v-screen? (not v-screen?)))

      (when h-pixel-flip? (set h-pixel? (not h-pixel?)))
      (when v-pixel-flip? (set v-pixel? (not v-pixel?)))

      (const bool screen-start? (and h-screen-start? v-screen-start?))
      (const bool pixel-start? (and h-pixel-start? v-pixel-start?))

      (when pixel-start? (set pixel-ref pixels)))))
