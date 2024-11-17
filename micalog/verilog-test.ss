(import
  (except (micascheme) write)
  (micalog keywords)
  (micalog verilog)
  (code))

(check (equal? (code-string (identifier->code #'clock)) "clock"))
(check (equal? (code-string (identifier->code #'item-counter)) "item_counter"))

(check (equal? (code-string (value->code #'128)) "128"))
(check (equal? (code-string (value->code #'clock)) "clock"))
(check (equal? (code-string (value->code #'(+ a b))) "a + b"))

(check (equal? (code-string (edge->code #'positive-edge)) "posedge"))
(check (equal? (code-string (edge->code #'negative-edge)) "negedge"))

(check (equal? (code-string (event->code #'(positive-edge clock))) "posedge clock"))
(check (equal? (code-string (event->code #'(negative-edge clock))) "negedge clock"))

(check (equal? (code-string (size->code #'8)) "[7:0]"))

(check
  (equal?
    (code-string
      (item->code
        #`(register counter
          (bit-count 8)
          (init 128)
          (on (positive-edge clock))
          (write (+ counter 1)))))
    (lines-string
      "reg [7:0] counter = 128;"
      "always @(posedge clock) begin"
      "  counter <= counter + 1;"
      "end")))
