(library (micac lib std)
  (export
    bool int uint8_t uint16_t uint32_t uint64_t
    sizeof alloc repeat for-each !=
    inc dec printf zero? not-zero? in-range? << >> xor)
  (import (micac))

  (micac
    (externs
      bool int uint8_t uint16_t uint32_t uint64_t
      malloc free sizeof printf)

    (macro (alloc id type size)
      (var type (* id) (cast (* type) (malloc (* size (sizeof type)))))
      (break-if (= id 0) (printf "Could not allocate memory.\\n"))
      (defer (free id)))

    (macro (inc id) (set id + 1))
    (macro (dec id) (set id - 1))

    (macro (!= a b) (not (= a b)))

    (macro (repeat count body ...)
      (for-each (index count) body ...))

    (macro (for-each (index count) body ...)
      (var int index 0)
      (while (!= counter count)
        body ...
        (inc index)))

    (macro (zero? x) (= x 0))
    (macro (not-zero? x) (!= x 0))

    (macro (>> x n) (bitwise-arithmetic-shift-right x n))
    (macro (<< x n) (bitwise-arithmetic-shift-left x n))

    (macro (in-range? x min max)
      (and (>= x min) (< x max)))

    (macro (xor xs ...) (bitwise-xor xs ...))
))
