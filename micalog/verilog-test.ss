(import
  (micalog keywords)
  (micalog verilog)
  (code))

(check-verilog (identifier clock) "clock")
(check-verilog (identifier item-counter) "item_counter")

(check-verilog (value 128) "128")
(check-verilog (value clock) "clock")
(check-verilog (value (+ a b)) "a + b")

(check-verilog (edge positive-edge) "posedge")
(check-verilog (edge negative-edge) "negedge")

(check-verilog (event (positive-edge clock)) "posedge clock")
(check-verilog (event (negative-edge clock)) "negedge clock")

(check-verilog (size 8) "[7:0]")

(check-verilog
  (item
    (register counter
      (size #f)
      (initial #f)
      (on (positive-edge clock))
      (if #f)
      (set (+ counter 1))))
  (lines
    "reg counter;"
    "always @(posedge clock) begin"
    "  counter <= counter + 1;"
    "end"))

(check-verilog
  (item
    (register counter
      (size 8)
      (initial 128)
      (on (positive-edge clock))
      (if enabled)
      (set (+ counter 1))))
  (lines
    "reg [7:0] counter = 128;"
    "always @(posedge clock) begin"
    "  if (enabled) begin"
    "    counter <= counter + 1;"
    "  end"
    "end"))

(check-verilog
  (item
    (wire next-value
      (size #f)
      (+ value 1)))
  (lines
    "wire next_value = value + 1;"))

(check-verilog
  (item
    (wire next-value
      (size 8)
      (+ value 1)))
  (lines
    "wire [7:0] next_value = value + 1;"))

(check-verilog
  (program
    (circuit
      (register counter
        (size 8)
        (initial 128)
        (on (positive-edge clock))
        (if #f)
        (set next-counter))
      (wire next-value
        (size 8)
        (+ counter 1))
      (register next-counter
        (size 8)
        (initial #f)
        (on (negative-edge clock))
        (if #f)
        (set next-value))))
  (lines
    "reg [7:0] counter = 128;"
    "always @(posedge clock) begin"
    "  counter <= next_counter;"
    "end"
    ""
    "wire [7:0] next_value = counter + 1;"
    ""
    "reg [7:0] next_counter;"
    "always @(negedge clock) begin"
    "  next_counter <= next_value;"
    "end"))
