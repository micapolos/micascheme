(library (micac std)
  (export sizeof alloc repeat-times)
  (import (micascheme) (micac c))

  (micac-externs malloc free sizeof)

  (micac-macro (alloc id type size)
    (var type (* id) (cast (* type) (malloc (* size (sizeof type)))))
    (break-if (= id 0) (printf "malloc error\\n"))
    (defer (free id)))

  (micac-macro (repeat-times count body ...)
    (begin
      (var int counter count)
      (while counter
        body ...
        (sub counter 1))))
)
