and
  native "string-append"
  as
    string
    append string
    promising string
and first "Hello"
and second "world"
do
  get: first, string
  append ", "
  append get: second, string
  append "!"
