module sandbox (
  input clock,
  input reset_,
  input mouse_pressed_,
  input [15:0] mouse_x,
  output [15:0] value
);
  assign value = counter;
  reg half_clock = 0;
  always @(posedge clock) begin
    half_clock <= inv_half_clock;
  end
  reg previous_half_clock = 0;
  always @(negedge clock) begin
    previous_half_clock <= half_clock;
  end
  wire next_half_clock;
  assign next_half_clock = ~previous_half_clock;
  reg [15:0] counter;
  always @(posedge half_clock) begin
    counter <= next_counter;
  end
  reg [15:0] previous_counter;
  always @(negedge half_clock) begin
    previous_counter <= counter;
  end
  wire [15:0] inc_counter;
  assign inc_counter = previous_counter + 1;
  wire [15:0] dec_counter;
  assign dec_counter = previous_counter - 1;
  wire [15:0] updated_counter;
  assign updated_counter = mouse_pressed_ ? inc_counter : dec_counter;
  wire [15:0] next_counter;
  assign next_counter = reset_ ? mouse_x : updated_counter;
endmodule
