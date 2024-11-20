module sandbox (
  input clock,
  input reset_,
  input mouse_pressed_,
  input [15:0] mouse_x,
  output [15:0] value
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
  assign value = counter;
endmodule
