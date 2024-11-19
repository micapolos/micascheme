(library (verilog string)
  (export verilog-string)
  (import (micascheme) (code) (verilog code))
  (export (import (verilog keywords)))

  (define-rule-syntax (verilog-string module)
    (fluent
      #'module
      (module->code)
      (code-string)))
)
