(import
  (micac)
  (micac lib std)
  (micac lib emu)
  (micac lib sdl)
  (micac zx-spectrum background)
  (micac zx-spectrum ula)
  (micac misc plasma)
  (micac run)
  (c run))

(c-run-echo? #t)
(micac-run-echo? #t)

(run-emu
  (video 352 288 96 24 4) ; width height h-blank v-blank cycles-per-pixel

  (var int frame-counter 0)

  (file scr scr-size "/Users/micapolos/git/micascheme/micac/scr/Cobra.scr")

  (background 4630 background-red background-green background-blue background-update)
  (ula video-x video-y scr ula-screen? ula-red ula-green ula-blue ula-update)
  (plasma video-x video-y frame-counter plasma-red plasma-green plasma-blue plasma-update)

  (update
    (when (zero? pixel-cycle-counter)
      (background-update)
      (ula-update)
      (plasma-update)

      (if ula-screen?
        (then
          (const bool plasma?
            (or
              (and (>= video-x mouse-x) (>= video-y mouse-y))
              (and (< video-x mouse-x) (< video-y mouse-y))))
          (if (xor plasma? mouse-pressed?)
            (then
              (set red ula-red)
              (set green ula-green)
              (set blue ula-blue))
            (else
              (set red plasma-red)
              (set green plasma-green)
              (set blue plasma-blue))))
        (else
          (set red background-red)
          (set green background-green)
          (set blue background-blue)))

      (const bool frame-start?
        (and (= video-x 0) (= video-y 0)))

      (when frame-start? (inc frame-counter)))))
