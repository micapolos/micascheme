(library (micalog verilog-string)
  (export verilog-string)
  (import
    (only (micascheme) fluent define-case-syntax literal->syntax syntax export ...)
    (only (code) code-string)
    (only (verilog code) module->code)
    (only (micalog verilog) module->verilog)
    (micalog keywords))
  (export (import (micalog keywords)))

  (define-case-syntax (verilog-string body)
    (fluent #'body
      (module->verilog)
      (module->code)
      (code-string)
      (literal->syntax)))
)
