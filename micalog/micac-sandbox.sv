module colour_bars (
  input clock,
  input [8:0] video_x,
  input [8:0] video_y,
  input [8:0] mouse_x,
  input [8:0] mouse_y,
  input mouse_pressed_,
  output [7:0] red,
  output [7:0] green,
  output [7:0] blue
);
  reg [63:0] red_counter;
  reg [63:0] green_counter;
  reg [63:0] blue_counter;
  reg [7:0] bar_red;
  reg [7:0] bar_green;
  reg [7:0] bar_blue;
  always @(posedge clock) begin
    red_counter <= red_counter + 1;
    green_counter <= green_counter + 1;
    blue_counter <= blue_counter + 1;
    if (red_counter > 'b100110111100100) begin
      red_counter <= 0;
      bar_red <= ~bar_red;
    end
    if (green_counter > 'b100110111010000) begin
      green_counter <= 0;
      bar_green <= ~bar_green;
    end
    if (blue_counter > 'b100110110111100) begin
      blue_counter <= 0;
      bar_blue <= ~bar_blue;
    end
  end
  wire screen_;
  assign screen_ = video_x >= 'b110000 & video_x < 'b100110000 & (video_y >= 'b110000 & video_y < 'b11110000);
  assign red = screen_ ? 'b11011101 : bar_red;
  assign green = screen_ ? 'b11011101 : bar_green;
  assign blue = screen_ ? 'b11011101 : bar_blue;
endmodule
