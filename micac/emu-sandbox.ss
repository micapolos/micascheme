(import
  (micac)
  (micac std)
  (micac emu)
  (micac sdl)
  (micac ula)
  (c run))

(c-run-echo? #t)

(run-emu
  (video 352 288 96 24 4) ; width height h-blank v-blank cycles-per-pixel

  (const int bar-size 4630)
  (var int bar-counter 0)

  (var uint8_t background-red #xff)
  (var uint8_t background-green #xff)
  (var uint8_t background-blue 0)

  (var int frame-counter 0)

  (var uint8_t bits)
  (var uint8_t attr)

  (file scr scr-size "/Users/micapolos/git/micascheme/micac/scr/Cobra.scr")

  (ula video-x video-y scr screen? ula-red ula-green ula-blue ula-update)

  (update
    (when (zero? pixel-cycle-counter)
      (ula-update)

      (if screen?
        (then
          (const bool ula?
            (or
              (and (>= video-x mouse-x) (>= video-y mouse-y))
              (and (< video-x mouse-x) (< video-y mouse-y))))
          (if (xor ula? mouse-pressed?)
            (then
              (set red ula-red)
              (set green ula-green)
              (set blue ula-blue))
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
