(import
  (micac c)
  (micac std)
  (micac run)
  (micac emu))

(micac-run
  (emu
    (video 352 288)
    (init
      (var int frame-counter 0))
    (update
      (var uint8_t (* pixel-ref) pixels)
      (var uint8_t value (* frame-counter 8))
      (repeat-times pixels-size
        (set (pixel-ref *) value)
        (add pixel-ref 1)
        (add value 1))
      (add frame-counter 1))))
