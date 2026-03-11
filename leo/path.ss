(library (leo path)
  (export leo-path-extension leo-path path-leo?)
  (import (micascheme))

  (define leo-path-extension "leo")

  (define (leo-path $components)
    (string-append
      (apply string-append (intercalate $components "/"))
      "."
      leo-path-extension))

  (define (path-leo? $path)
    (string=? (path-extension $path) leo-path-extension))
)
