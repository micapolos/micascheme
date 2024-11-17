reg [7:0] counter = 0;
always @(posedge clock) begin
  if (write_) begin
    counter <= counter + 1;
  end
end
