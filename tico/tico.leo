and
  native "string-append"
  as
    string
    append string
    promising string
and
  native "string-length"
  as
    string
    offering length number
and
  native "number->string"
  as
    number
    offering string
and first "Hello"
and second "world"
do
  get: first, string
  append ", "
  append get: second, string
  append "!"
