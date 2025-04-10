(library (micac zx-spectrum background)
  (export background)
  (import (micac) (micac lib std))

  (micac
    (macro (background size red green blue update)
      (var int bar-counter 0)
      (var uint8_t red #xff)
      (var uint8_t green #xff)
      (var uint8_t blue 0)
      (macro (update)
        (inc bar-counter)
        (when (= bar-counter size)
          (set bar-counter 0)
          (set red (bitwise-not red))
          (set green (bitwise-not green))
          (set blue (bitwise-not blue)))))))
