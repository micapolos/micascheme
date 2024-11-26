(import
  (prefix (micascheme) %)
  (micalog)
  (only (micalog verilog) verilog-string))

(micalog
  (macro (double expr) (+ expr expr)))

(%check
  (%equal?
    (verilog-string
      (module foo
        (input 4 in)
        (output out (double in))))
    (%lines-string
      "module foo ("
      "  input [3:0] in,"
      "  output [4:0] out"
      ");"
      "  assign out = in + in;"
      "endmodule")))
