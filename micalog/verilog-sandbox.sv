reg half_clock;
always @(posedge clock) begin
  half_clock <= ~half_clock;
end
reg [7:0] counter = 0;
always @(posedge half_clock) begin
  if (write_) begin
    counter <= counter + 1;
  end
end
