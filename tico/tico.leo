comment
  this is a comment
  numbers: 10, 20, 30
  strings: "foo", "bar"
take
  native "string-append"
  as
    string
    append string
    promising string
take
  native "string-length"
  as
    string
    offering length number
take
  native "number->string"
  as
    number
    offering string
take first "Hello"
take second "world"
type
  point
    x number
    y number
do
  get: first, string
  append ", "
  append get: second, string
  append "!"
