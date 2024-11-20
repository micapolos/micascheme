(library (micalog verilog-string)
  (export verilog-string)
  (import
    (only (micascheme) fluent define-case-syntax literal->syntax syntax export ...)
    (only (code) code-string)
    (rename
      (only (verilog code) module->code)
      (module->code verilog-module->code))
    (rename
      (only (micalog verilog) module->verilog)
      (module->verilog micalog-module->verilog))
    (only (micalog model) flatten-module)
    (micalog keywords))
  (export (import (micalog keywords)))

  (define-case-syntax (verilog-string body)
    (fluent #'body
      (flatten-module)
      (micalog-module->verilog)
      (verilog-module->code)
      (code-string)
      (literal->syntax)))
)
