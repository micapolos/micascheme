(library (micac misc plasma)
  (export plasma)
  (import
    (micac)
    (micac lib std))

  (micac
    (macro (plasma video-x video-y counter red green blue update)
      (var uint8_t red)
      (var uint8_t green)
      (var uint8_t blue)
      (macro (update)
        (set red (- counter video-x))
        (set green (- counter video-y))
        (set blue (+ counter (>> (* video-x video-y) 6)))))))
