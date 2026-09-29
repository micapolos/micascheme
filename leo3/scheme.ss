(library (leo3 scheme)
  (export)
  (import
    (rename (scheme)
      (import %import)
      (export %export)
      (library %library))
    (leo3 core))
  (%export
    (import
      (rename
        (except (scheme) import export library)
        (+ add)
        (- subtract)
        (* multiply))
      (leo3 core)))
)
