reg half_clock = 0;
always @(posedge clock) begin
  half_clock <= ~half_clock;
end
reg [7:0] counter_8 = 0;
always @(posedge half_clock) begin
  if (write_) begin
    counter_8 <= counter + 1;
  end
end
wire [3:0] counter_4 = counter_8 & 15;
