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
      (except (scheme) import export library)
      (leo3 core)))
)
