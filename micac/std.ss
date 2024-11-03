(library (micac std)
  (export sizeof create-array)
  (import (micascheme) (micac c))

  (micac-externs malloc free sizeof)

  (micac-macro (create-array type id size)
    (var type (* id) (cast (* type) (malloc (* size (sizeof type)))))
    (break-if (= id 0)
      (printf "malloc error\\n"))
    (defer (free id)))
)
