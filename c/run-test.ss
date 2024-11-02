(import (micascheme) (c run))

(check
  (equal?
    (c-run
      (lines-string
        "int main() {"
        "  return 128;"
        "}"))
    128))
