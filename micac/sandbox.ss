(import (micascheme) (micac c-run) (micac run))

(c-run
  (lines-string
    "#include <stdio.h>"
    "int main() {"
    "  printf(\"Hello, world!\\n\");"
    "}"));

(micac-run
  (var u8 x)
  (add x 2)
  (print first-value x)
  (add x 3)
  (print second-value x))
