(library (micac std)
  (export sizeof alloc repeat inc dec printf zero?)
  (import (micac))

  (micac
    (externs malloc free sizeof printf)

    (macro (alloc id type size)
      (var type (* id) (cast (* type) (malloc (* size (sizeof type)))))
      (break-if (= id 0) (printf "Could not allocate memory.\\n"))
      (defer (free id)))

    (macro (inc id) (set+ id 1))
    (macro (dec id) (set- id 1))

    (macro (repeat count body ...)
      (begin
        (var int counter count)
        (while counter
          body ...
          (dec counter))))

    (macro (zero? x) (= x 0))))
