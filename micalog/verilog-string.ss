(library (micalog verilog-string)
  (export verilog-string)
  (import
    (only (micascheme) define-case-syntax literal->syntax syntax export ...)
    (only (code) code-string)
    (only (verilog code) module->code)
    (only (micalog verilog) module->verilog)
    (micalog keywords))
  (export (import (micalog keywords)))

  (define-case-syntax (verilog-string body)
    (literal->syntax
      (code-string
        (module->code
          (module->verilog #'body)))))
)
