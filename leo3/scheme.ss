(library (leo3 scheme)
  (export)
  (import
    (rename (scheme)
      (import %import)
      (export %export)
      (library %library)
      (top-level-program %top-level-program))
    (leo3 core))
  (%export
    (import
      (rename
        (except (scheme) import export library top-level-program define)
        (+ add)
        (- subtract)
        (* multiply))
      (leo3 core)
      (leo3 define)))
)
