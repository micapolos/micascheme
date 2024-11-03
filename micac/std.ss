(library (micac std)
  (export sizeof alloc repeat-times inc dec set* on)
  (import (micascheme) (micac c))

  (micac-externs malloc free sizeof)

  (micac-macro (alloc id type size)
    (var type (* id) (cast (* type) (malloc (* size (sizeof type)))))
    (break-if (= id 0) (printf "Could not allocate memory\\n"))
    (defer (free id)))

  (micac-macro (repeat-times count body ...)
    (begin
      (var int counter count)
      (while counter
        body ...
        (sub counter 1))))

  (micac-macro (inc id) (add id 1))
  (micac-macro (dec id) (sub id 1))
  (micac-macro (set* id expr) (set (id *) expr))

  (micac-macro (on expr body ...)
    (if expr (begin body ...)))
)
