(import
  (except (micascheme) write)
  (micalog keywords)
  (micalog verilog)
  (code))

(define-case-syntax (check-verilog (id body) string)
  #`(check
    (equal?
      (code-string
        (#,(identifier-append #'id #'id #'->code) #'body))
      string)))

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
      (bit-count 8)
      (init 128)
      (on (positive-edge clock))
      (write (+ counter 1))))
  (lines-string
    "reg [7:0] counter = 128;"
    "always @(posedge clock) begin"
    "  counter <= counter + 1;"
    "end"))

(check-verilog
  (program
    (circuit
      (register counter
        (bit-count 8)
        (init 128)
        (on (positive-edge clock))
        (write next-counter))
      (register next-counter
        (bit-count 8)
        (init 0)
        (on (negative-edge clock))
        (write (+ counter 1)))))
  (lines-string
    "reg [7:0] counter = 128;"
    "always @(posedge clock) begin"
    "  counter <= next_counter;"
    "end"
    ""
    "reg [7:0] next_counter = 0;"
    "always @(negedge clock) begin"
    "  next_counter <= counter + 1;"
    "end"))
