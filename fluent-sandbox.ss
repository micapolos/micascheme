(import (micascheme))

(fluent
  "foo"
  (string-append "bar")
  (do $string
    $string
    (string-append $string))
  (displayln))
