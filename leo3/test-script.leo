import: scheme:
define: hello, "Hello"
define: world, "world"
define:
  comma separated: first, second
  string append: first, ", ", second
define:
  exclamated: string
  string append: string, "!"
define:
  display line: string
  display: string
  newline:
display line: exclamated: comma separated: hello, world
