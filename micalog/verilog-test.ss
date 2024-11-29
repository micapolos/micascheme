(import
  (micalog verilog)
  (prefix (micascheme) %))

(%check-equal?
  (verilog-string (module foo))
  (%lines-string
    "module foo ();"
    "endmodule"))
