(import (micascheme) (simplang expander) (simplang core))

(parameterize
  ((current-expand
    (lambda ($obj $env . $params)
      (apply sc-expand
        `(pretty-print ,(cdr (typed core-scope $obj)))
        $env $params))))
  (load "simplang/demo.ss"))
