(import
  (verilog keywords)
  (verilog code)
  (code))

(check-verilog (io input) "input")
(check-verilog (io output) "output")

(check-verilog (parameter (input foo)) "input foo")
(check-verilog (parameter (input (7 to 0) foo)) "input [7:0] foo")
(check-verilog (parameter (output bar)) "output bar")
(check-verilog (parameter (output (7 to 0) bar)) "output [7:0] bar")

(check-verilog (name clock) "clock")
(check-verilog (name item-counter) "item_counter")
(check-verilog (name reset?) "reset_")
(check-verilog (name wire) "_wire")
(check-verilog (name initial) "_initial")

(check-verilog (expr #b10010) "'b10010")
(check-verilog (expr clock) "clock")
(check-verilog (expr "foo") "\"foo\"")

(check-verilog (expr (= a b)) "a == b")
(check-verilog (expr (!= a b)) "a != b")
(check-verilog (expr (< a b)) "a < b")
(check-verilog (expr (<= a b)) "a <= b")
(check-verilog (expr (> a b)) "a > b")
(check-verilog (expr (>= a b)) "a >= b")

(check-verilog (expr (+ a b)) "a + b")
(check-verilog (expr (- a)) "-a")
(check-verilog (expr (- a b)) "a - b")
(check-verilog (expr (* a b)) "a * b")

(check-verilog (expr (not a)) "~a")

(check-verilog (expr (and a b)) "a & b")
(check-verilog (expr (or a b)) "a | b")
(check-verilog (expr (xor a b)) "a ^ b")
(check-verilog (expr (nand a b)) "a ~& b")
(check-verilog (expr (nor a b)) "a ~| b")
(check-verilog (expr (xnor a b)) "a ^~ b")

(check-verilog (expr (ref a)) "a")
(check-verilog (expr (ref a (12))) "a[12]")
(check-verilog (expr (ref a (12) (13))) "a[12][13]")
(check-verilog (expr (ref a (3 to 0))) "a[3:0]")

(check-verilog (expr (if a b c)) "a ? b : c")

(check-verilog (expr (append)) "{}")
(check-verilog (expr (append a)) "{ a }")
(check-verilog (expr (append a b)) "{ a, b }")
(check-verilog (expr (append a b c)) "{ a, b, c }")

(check-verilog (expr (- (- a b) (- c d))) "a - b - (c - d)")
(check-verilog (expr (or (and a b) (and c d))) "a & b | c & d")
(check-verilog (expr (and (or a b) (or c d))) "(a | b) & (c | d)")

(check-verilog
  (statement (set! x y))
  (lines "x <= y;"))

(check-verilog
  (statement (assign x y))
  (lines "assign x = y;"))

(check-verilog
  (statement
    (cond
      (reset (set! x 0))
      (write (set! x data))))
  (lines
    "if (reset) begin"
    "  x <= 0;"
    "end else if (write) begin"
    "  x <= data;"
    "end"))

(check-verilog
  (statement
    (cond
      (reset (set! x 0))
      (write (set! x data))
      (else (set! x default))))
  (lines
    "if (reset) begin"
    "  x <= 0;"
    "end else if (write) begin"
    "  x <= data;"
    "end else begin"
    "  x <= _default;"
    "end"))

(check-verilog
  (statement (display "result: %d" (+ foo bar)))
  (lines "$display(\"result: %d\", foo + bar);"))

(check-verilog (edge posedge) "posedge")
(check-verilog (edge negedge) "negedge")

(check-verilog (event *) "*")
(check-verilog (event (negedge clock)) "negedge clock")
(check-verilog (event (negedge clock)) "negedge clock")

(check-verilog (size 8) "[7:0]")

(check-verilog
  (declaration (wire foo))
  (lines "wire foo;"))

(check-verilog
  (declaration (wire (3 to 0) foo))
  (lines "wire [3:0] foo;"))

(check-verilog
  (declaration (wire (3 to 0) foo (4 to 0)))
  (lines "wire [3:0] foo [4:0];"))

(check-verilog
  (declaration (reg foo))
  (lines "reg foo;"))

(check-verilog
  (declaration (reg (3 to 0) foo))
  (lines "reg [3:0] foo;"))

(check-verilog
  (declaration (reg (3 to 0) foo (4 to 0)))
  (lines "reg [3:0] foo [4:0];"))

(check-verilog
  (declaration
    (always *
      (assign x z)
      (assign y z)))
  (lines
    "always @(*) begin"
    "  assign x = z;"
    "  assign y = z;"
    "end"))

(check-verilog
  (declaration
    (always (posedge clock)
      (set! x z)
      (set! y z)))
  (lines
    "always @(posedge clock) begin"
    "  x <= z;"
    "  y <= z;"
    "end"))

(check-verilog
  (declaration (assign a b))
  (lines "assign a = b;"))

(check-verilogs
  (declarations
    (reg (7 to 0) counter)
    (wire (7 to 0) counter-next)
    (always *
      (assign counter-next (+ counter 1)))
    (always (negedge clock)
      (set! counter counter-next)))
  (lines
    "reg [7:0] counter;"
    "wire [7:0] counter_next;"
    "always @(*) begin"
    "  assign counter_next = counter + 1;"
    "end"
    "always @(negedge clock) begin"
    "  counter <= counter_next;"
    "end"))

(check-verilog
  (module (module (foo)))
  (lines
    "module foo ();"
    "endmodule"))

(check-verilog
  (module
    (module (foo (input in) (output out-1) (output out-2))
      (always *
        (assign out-1 in)
        (assign out-2 in))))
  (lines
    "module foo ("
    "  input in,"
    "  output out_1,"
    "  output out_2"
    ");"
    "  always @(*) begin"
    "    assign out_1 = in;"
    "    assign out_2 = in;"
    "  end"
    "endmodule"))
