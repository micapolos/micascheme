(import
  (micac c)
  (micac std)
  (micac run)
  (micac emu))

(micac-run
  (emu
    (clock (* 352 288 4 60))
    (video 352 288)
    (init
      (var uint8_t (* pixel-ref) pixels)
      (var int pixel-counter pixels-size)
      (var int frame-counter 0)
      (var uint8_t pixel))
    (update
      (set (pixel-ref *) pixel)
      (add pixel-ref 1)
      (add pixel 1)
      (sub pixel-counter 1)
      (if (= pixel-counter 0)
        (begin
          (set pixel-ref pixels)
          (set pixel-counter pixels-size)
          (add frame-counter 1)
          (set pixel (* frame-counter 8)))))))
