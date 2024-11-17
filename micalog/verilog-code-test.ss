(import
  (micalog keywords)
  (micalog verilog-code)
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
(check-verilog (expr (ref a (3 0))) "a[3:0]")

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

(check-verilog (edge positive-edge) "posedge")
(check-verilog (edge negative-edge) "negedge")

(check-verilog (event (positive-edge clock)) "posedge clock")
(check-verilog (event (negative-edge clock)) "negedge clock")

(check-verilog (size 8) "[7:0]")

(check-verilog
  (declaration (wire foo bit))
  (lines "wire foo;"))

(check-verilog
  (declaration (wire foo (vector bit 8)))
  (lines "wire [7:0] foo;"))

(check-verilog
  (declaration (wire foo bit x))
  (lines "wire foo = x;"))

(check-verilog
  (declaration (wire foo (vector bit 8) x))
  (lines "wire [7:0] foo = x;"))

(check-verilog
  (declaration (wire foo (vector (vector bit 8) 4) x))
  (lines "wire [7:0] foo [3:0] = x;"))

(check-verilog
  (declaration (wire foo (vector (vector (vector bit 8) 4) 3) x))
  (lines "wire [7:0] foo [3:0][2:0] = x;"))

(check-verilog
  (declaration (reg foo bit))
  (lines "reg foo;"))

(check-verilog
  (declaration (reg foo (vector bit 8)))
  (lines "reg [7:0] foo;"))

(check-verilog
  (declaration (reg foo bit x))
  (lines "reg foo = x;"))

(check-verilog
  (declaration (reg foo (vector bit 8) x))
  (lines "reg [7:0] foo = x;"))

(check-verilog
  (declaration (reg foo (vector (vector bit 8) 4) x))
  (lines "reg [7:0] foo [3:0] = x;"))

(check-verilog
  (declaration (reg foo (vector (vector (vector bit 8) 4) 3) x))
  (lines "reg [7:0] foo [3:0][2:0] = x;"))

(check-verilog
  (declaration
    (always (positive-edge clock)
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
      (reg counter (vector bit 8) 128)
      (wire counter-next (vector bit 8) (+ counter 1))
      (always (negative-edge clock)
        (set! counter counter-next))))
  (lines
    "reg [7:0] counter = 128;"
    "wire [7:0] counter_next = counter + 1;"
    "always @(negedge clock) begin"
    "  counter <= counter_next;"
    "end"))
