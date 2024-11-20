module funny_counter (
  input clock,
  input reset_,
  input mouse_pressed_,
  input [15:0] mouse_x,
  output [8:0] value
);
  reg half_clock;
  reg [7:0] counter;
  always @(posedge clock) begin
    half_clock <= ~half_clock;
  end
  always @(posedge half_clock) begin
    if (reset_) begin
      counter <= mouse_x[7:0];
    end else if (mouse_pressed_) begin
      counter <= counter + 1;
    end else begin
      counter <= counter - 1;
    end
  end
  assign value = counter;
endmodule
