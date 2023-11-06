native "string-append"
as
  string
  append string
  giving string
x "Hello, "
y "world!"
do
  get
    string
    append string
    giving string
  apply
    get: x, string
    append get: y, string
