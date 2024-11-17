(library (micalog verilog)
  (export verilog)
  (import (micascheme) (code) (micalog verilog-code))
  (export (import (micalog keywords)))

  (define-rule-syntax (verilog declaration ...)
    (fluent
      (syntaxes declaration ...)
      (declarations->code)
      (code-string)
      (display)))
)
