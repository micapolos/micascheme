module and_gate (
  input in_1,
  input in_2,
  output out_and,
  output out_nand
);
  assign out_and = in_1 & in_2;
  assign out_nand = ~out_and;
endmodule

module clock_counter (
  input clock,
  output [3:0] counter
);
  reg clock_1;
  reg clock_2;
  reg clock_3;
  always @(posedge clock) begin
    clock_1 <= ~clock_1;
  end
  always @(posedge clock_1) begin
    clock_2 <= ~clock_2;
  end
  always @(posedge clock_2) begin
    clock_3 <= ~clock_3;
  end
  assign counter = {clock_3, clock_2, clock_1, clock};
endmodule

module funny_counter (
  input clock,
  input reset_,
  input mouse_pressed_,
  input [15:0] mouse_x,
  output [15:0] out
);
  reg half_clock;
  reg [15:0] counter;
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
