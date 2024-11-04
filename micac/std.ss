(library (micac std)
  (export sizeof alloc repeat inc dec on printf)
  (import (micac))

  (micac
    (externs malloc free sizeof printf)

    (macro (alloc id type size)
      (var type (* id) (cast (* type) (malloc (* size (sizeof type)))))
      (break-if (= id 0) (printf "Could not allocate memory\\n"))
      (defer (free id)))

    (macro (repeat count body ...)
      (begin
        (var int counter count)
        (while counter
          body ...
          (sub counter 1))))

    (macro (inc id)
      (add id 1))

    (macro (dec id)
      (sub id 1))

    (macro (on expr body ...)
      (if expr (begin body ...))))
)
