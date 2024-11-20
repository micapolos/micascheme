module and_gate (
  input in_1,
  input in_2,
  output out_and,
  output out_nand
);
  assign out_and = in_1 & in_2;
  assign out_nand = ~out_and;
endmodule

module funny_counter (
  input clock,
  input reset_,
  input mouse_pressed_,
  input [15:0] mouse_x,
  output [15:0] out
);
  reg half_clock = 0;
  reg [15:0] counter = 0;
  wire [15:0] inc_counter;
  assign inc_counter = counter + 1;
  wire [15:0] dec_counter;
  assign dec_counter = counter - 1;
  wire [15:0] updated_counter;
  assign updated_counter = mouse_pressed_ ? inc_counter : dec_counter;
  always @(posedge clock) begin
    half_clock <= ~half_clock;
    counter <= reset_ ? mouse_x : updated_counter;
  end
  assign out = counter;
endmodule
