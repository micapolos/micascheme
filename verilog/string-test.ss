(import
  (only (micascheme) check equal? lines-string)
  (verilog string))

(check
  (equal?
    (verilog-string (module (foo (input x) (output y))))
    (lines-string
      "module foo ("
      "  input x,"
      "  output y"
      ");"
      "endmodule")))
