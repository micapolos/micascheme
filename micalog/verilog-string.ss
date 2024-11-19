(library (micalog verilog-string)
  (export micalog->verilog-string)
  (import
    (only (micascheme) define-case-syntax literal->syntax syntax)
    (only (code) code-string)
    (only (micalog keywords) micalog)
    (only (verilog code) module->code)
    (only (micalog verilog) module->verilog))

  (define-case-syntax (micalog->verilog-string module)
    (literal->syntax (code-string (module->code (module->verilog #'module)))))
)
