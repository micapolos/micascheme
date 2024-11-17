reg [7:0] counter = 128;
always @(posedge clock) begin
  counter <= next_counter;
end
wire [7:0] next_value = counter + 1;
reg [7:0] next_counter;
always @(negedge clock) begin
  next_counter <= next_value;
end
