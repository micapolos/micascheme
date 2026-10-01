(library (leo3 read)
  (export read-file)
  (import
    (scheme)
    (lets)
    (prefix (mica reader) %)
    (prefix (leo3 reader line) %))

  (define (read-file $path)
    ;(pretty-print `(reading-leo ,$path))
    (%read-file (%inline?-line-annotations #f) $path))
)
