reg half_clock = 0;
reg [7:0] counter_8 = 0;
wire [3:0] counter_4 = counter_8[3:0];
always @(posedge clock) begin
  half_clock <= ~half_clock;
end
always @(posedge half_clock) begin
  if (write_) begin
    counter_8 <= counter_8 + 1;
  end
end
