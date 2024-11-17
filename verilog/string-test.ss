(import
  (only (micascheme) check equal? lines-string)
  (verilog string))

(check
  (equal?
    (verilog-string (wire x))
    (lines-string "wire x;")))
