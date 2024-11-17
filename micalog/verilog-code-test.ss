(import
  (micalog keywords)
  (micalog verilog-code)
  (code))

(check-verilog (name clock) "clock")
(check-verilog (name item-counter) "item_counter")

(check-verilog (expr 128) "128")
(check-verilog (expr clock) "clock")
(check-verilog (expr (+ a b)) "a + b")
(check-verilog (expr (and a b)) "a & b")
(check-verilog (expr (or a b)) "a | b")
(check-verilog (expr (not a)) "~a")

(check-verilog (expr (get a (12))) "a[12]")
(check-verilog (expr (get a (3 0))) "a[3:0]")

(check-verilog (expr (join a b)) "{a, b}")

(check-verilog (edge positive-edge) "posedge")
(check-verilog (edge negative-edge) "negedge")

(check-verilog (event (positive-edge clock)) "posedge clock")
(check-verilog (event (negative-edge clock)) "negedge clock")

(check-verilog (size 8) "[7:0]")

(check-verilog
  (declaration (foo bit))
  (lines "wire foo;"))

(check-verilog
  (declaration (foo (vector bit 8)))
  (lines "wire [7:0] foo;"))

(check-verilog
  (declaration (foo bit x))
  (lines "wire foo = x;"))

(check-verilog
  (declaration (foo (vector bit 8) x))
  (lines "wire [7:0] foo = x;"))

(check-verilog
  (declaration (foo (vector (vector bit 8) 4) x))
  (lines "wire [7:0] foo [3:0] = x;"))

(check-verilog
  (declaration (foo (vector (vector (vector bit 8) 4) 3) x))
  (lines "wire [7:0] foo [3:0][2:0] = x;"))

(check-verilog
  (declaration
    (foo
      (vector bit 8)
      (on (positive-edge clock) y)))
  (lines
    "reg [7:0] foo;"
    "always @(posedge clock) begin"
    "  foo <= y;"
    "end"))

(check-verilog
  (declaration
    (foo
      (vector bit 8)
      (initial x)
      (on (positive-edge clock) y)))
  (lines
    "reg [7:0] foo = x;"
    "always @(posedge clock) begin"
    "  foo <= y;"
    "end"))

(check-verilog
  (program
    (circuit
      (counter
        (vector bit 8)
        (initial 128)
        (on (positive-edge clock) next-counter))
      (next-value
        (vector bit 8)
        (+ counter 1))
      (next-counter
        (vector bit 8)
        (on (negative-edge clock) next-value))))
  (lines
    "reg [7:0] counter = 128;"
    "always @(posedge clock) begin"
    "  counter <= next_counter;"
    "end"
    "wire [7:0] next_value = counter + 1;"
    "reg [7:0] next_counter;"
    "always @(negedge clock) begin"
    "  next_counter <= next_value;"
    "end"))
