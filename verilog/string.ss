(library (verilog string)
  (export verilog-string)
  (import (micascheme) (code) (verilog code))
  (export (import (verilog keywords)))

  (define-rule-syntax (verilog-string declaration ...)
    (fluent
      (syntaxes declaration ...)
      (declarations->code)
      (code-string)))
)
