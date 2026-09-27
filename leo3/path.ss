(library (leo3 path)
  (export path-leo?)
  (import
    (scheme)
    (lets))

  (define (path-leo? $path)
    (lets
      ($path-extension (path-extension $path))
      (and $path-extension (string=? $path-extension "leo"))))
)
