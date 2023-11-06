x "Hello, "
y "world!"
do
  native "string-append"
  as
    string
    string
    giving string
  apply
    get: x, string
    take get: y, string
