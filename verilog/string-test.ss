(import
  (micascheme)
  (only (verilog string) verilog-string wire bit))

(check
  (equal?
    (verilog-string (wire x bit 0))
    (lines-string "wire x = 0;")))
