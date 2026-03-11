(library (leo path)
  (export
    leo-extension
    leo-library-extensions
    leo-path
    path-leo?)
  (import (micascheme))

  (define leo-extension "leo")

  (define leo-library-extensions
    `((,(string-append "." leo-extension) . ".so")))

  (define (leo-path $components)
    (string-append
      (apply string-append (intercalate $components "/"))
      "."
      leo-extension))

  (define (path-leo? $path)
    (string=? (path-extension $path) leo-extension))
)
