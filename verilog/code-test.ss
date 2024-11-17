(import
  (verilog keywords)
  (verilog code)
  (code))

(check-verilog (name clock) "clock")
(check-verilog (name item-counter) "item_counter")

(check-verilog (expr 128) "128")
(check-verilog (expr clock) "clock")

(check-verilog (expr (+)) "0")
(check-verilog (expr (+ a)) "a")
(check-verilog (expr (+ a b)) "a + b")
(check-verilog (expr (+ a b c)) "a + b + c")

(check-verilog (expr (and)) "~0")
(check-verilog (expr (and a)) "a")
(check-verilog (expr (and a b)) "a & b")
(check-verilog (expr (and a b c)) "a & b & c")

(check-verilog (expr (or)) "0")
(check-verilog (expr (or a)) "a")
(check-verilog (expr (or a b)) "a | b")
(check-verilog (expr (or a b c)) "a | b | c")

(check-verilog (expr (inv a)) "~a")

(check-verilog (expr (ref a)) "a")
(check-verilog (expr (ref a (12))) "a[12]")
(check-verilog (expr (ref a (12) (13))) "a[12][13]")
(check-verilog (expr (ref a (3 : 0))) "a[3:0]")

(check-verilog (expr (append)) "{}")
(check-verilog (expr (append a)) "{a}")
(check-verilog (expr (append a b)) "{a, b}")

(check-verilog
  (statement (set! x y))
  (lines "x <= y;"))

(check-verilog
  (statement
    (if x
      (set! x 10)
      (set! y 20)))
  (lines
    "if (x) begin"
    "  x <= 10;"
    "  y <= 20;"
    "end"))

(check-verilog (edge posedge) "posedge")
(check-verilog (edge negedge) "negedge")

(check-verilog (event (posedge clock)) "posedge clock")
(check-verilog (event (negedge clock)) "negedge clock")

(check-verilog (size 8) "[7:0]")

(check-verilog
  (declaration (wire foo))
  (lines "wire foo;"))

(check-verilog
  (declaration (wire (3 : 0) foo))
  (lines "wire [3:0] foo;"))

(check-verilog
  (declaration (wire (3 : 0) foo (4 : 0)))
  (lines "wire [3:0] foo [4:0];"))

(check-verilog
  (declaration (wire foo 0))
  (lines "wire foo = 0;"))

(check-verilog
  (declaration (wire (3 : 0) foo 0))
  (lines "wire [3:0] foo = 0;"))

(check-verilog
  (declaration (reg foo))
  (lines "reg foo;"))

(check-verilog
  (declaration (reg (3 : 0) foo))
  (lines "reg [3:0] foo;"))

(check-verilog
  (declaration (reg (3 : 0) foo (4 : 0)))
  (lines "reg [3:0] foo [4:0];"))

(check-verilog
  (declaration (reg foo 0))
  (lines "reg foo = 0;"))

(check-verilog
  (declaration (reg (3 : 0) foo 0))
  (lines "reg [3:0] foo = 0;"))

(check-verilog
  (declaration
    (always (posedge clock)
      (set! x 10)
      (set! y 20)))
  (lines
    "always @(posedge clock) begin"
    "  x <= 10;"
    "  y <= 20;"
    "end"))

(check-verilog
  (program
    (circuit
      (reg (7 : 0) counter 128)
      (wire (7 : 0) counter-next (+ counter 1))
      (always (negedge clock)
        (set! counter counter-next))))
  (lines
    "reg [7:0] counter = 128;"
    "wire [7:0] counter_next = counter + 1;"
    "always @(negedge clock) begin"
    "  counter <= counter_next;"
    "end"))
