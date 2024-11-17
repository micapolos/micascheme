(import
  (only (micascheme) check equal? lines-string)
  (verilog string))

(check
  (equal?
    (verilog-string (wire x bit 0))
    (lines-string "wire x = 0;")))
