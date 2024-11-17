(library (micalog syntax)
  (export verilog-string)
  (import (micascheme) (code) (micalog verilog))
  (export (import (micalog keywords)))

  (define-rule-syntax (verilog-string declaration ...)
    (code-string (declarations->code (syntaxes declaration ...))))
)
