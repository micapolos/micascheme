module empty ();
endmodule

module and_gate (
  input [7:0] in_1,
  input [7:0] in_2,
  output [7:0] out_and
);
  assign out_and = in_1 & in_2;
endmodule

module oscillating_registers (
  input clock,
  input reset_,
  input [1:0] _initial,
  output [1:0] out
);
  reg [1:0] reg_1;
  reg [1:0] reg_2;
  always @(posedge clock) begin
    if (reset_) begin
      reg_1 <= reg_2;
      reg_2 <= reg_1;
    end else begin
      reg_1 <= _initial;
      reg_1 <= ~_initial;
    end
  end
  assign out = reg_1;
endmodule

module counter_4 (
  input clock_0,
  output [3:0] counter
);
  reg clock_1;
  reg clock_2;
  reg clock_3;
  always @(posedge clock_0) begin
    clock_1 <= ~clock_1;
  end
  always @(posedge clock_1) begin
    clock_2 <= ~clock_2;
  end
  always @(posedge clock_2) begin
    clock_3 <= ~clock_3;
  end
  assign counter = { clock_3, clock_2, clock_1, clock_0 };
endmodule

module cascading_counter_4 (
  input clock_0,
  output [3:0] counter
);
  reg clock_1;
  reg clock_2;
  reg clock_3;
  always @(posedge clock_2) begin
    clock_3 <= ~clock_3;
  end
  always @(posedge clock_1) begin
    clock_2 <= ~clock_2;
  end
  always @(posedge clock_0) begin
    clock_1 <= ~clock_1;
  end
  assign counter = { clock_3, clock_2, clock_1, clock_0 };
endmodule

module funny_module (
  input clock,
  input reset_,
  input mouse_pressed_,
  input [15:0] mouse_x,
  output [15:0] out
);
  reg [15:0] counter;
  wire [15:0] inc_counter;
  assign inc_counter = counter + 1;
  wire [15:0] dec_counter;
  assign dec_counter = counter - 1;
  wire [15:0] updated_counter;
  assign updated_counter = mouse_pressed_ ? inc_counter : dec_counter;
  always @(posedge clock) begin
    counter <= reset_ ? mouse_x : updated_counter;
  end
  assign out = counter;
endmodule
