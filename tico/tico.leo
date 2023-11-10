load tico natives
take first "Hello"
take second "world"
do
  get: first, string
  append ", "
  append get: second, string
  append "!"
