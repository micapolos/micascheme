(import (micascheme))

(fluent
  "foo"
  (string-append "bar")
  (let $string
    $string
    (string-append $string))
  (displayln))
