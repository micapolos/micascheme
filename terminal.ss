(library (terminal)
  (export
    enter-raw-mode
    exit-raw-mode
    get-clipboard)
  (import (scheme))

  (define enter-raw-mode (foreign-procedure "(cs)ee_raw" () void))
  (define exit-raw-mode (foreign-procedure "(cs)ee_noraw" () void))
  (define get-clipboard (foreign-procedure "(cs)ee_get_clipboard" () scheme-object))
)
