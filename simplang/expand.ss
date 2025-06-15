(library (simplang expand)
  (export simplang-expand)
  (import (micascheme) (simplang expander) (simplang core))

  (define (simplang-expand $obj $env . $params)
    (apply sc-expand
      (cdr (typed core-scope $obj))
      $env $params))
)
