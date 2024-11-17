reg half_clock;
reg [7:0] counter_8;
wire [3:0] counter_4;
assign counter_4 = counter_8[3:0];
always @(posedge clock) begin
  half_clock <= ~half_clock;
end
always @(posedge half_clock) begin
  if (reset_) begin
    counter_8 <= 0;
  end else if (increment_) begin
    counter_8 <= counter_8 + 1;
  end
end
