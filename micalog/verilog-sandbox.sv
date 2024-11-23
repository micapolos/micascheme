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
      reg_1 <= _initial;
      reg_2 <= ~_initial;
    end else if (%else) begin
      reg_1 <= reg_2;
      reg_2 <= reg_1;
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
  always @(posedge clock) begin
    if (reset_) begin
      counter <= mouse_x;
    end else if (mouse_pressed_) begin
      counter <= counter + 1;
    end else if (%else) begin
      counter <= counter - 1;
    end
  end
  assign out = counter;
endmodule
