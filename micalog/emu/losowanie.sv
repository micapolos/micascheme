module losowanie (
  input clock,
  input reset_,
  input mouse_pressed_
);
  reg [1:0] counter;
  reg [1:0] result;
  reg done_;
  always @(posedge clock) begin
    if (reset_) begin
      counter <= 0;
      done_ <= 0;
    end else begin
      counter <= counter + 1[1:0];
      if (mouse_pressed_ & ~done_) begin
        result <= counter;
        done_ <= 1;
      end
    end
  end
endmodule
