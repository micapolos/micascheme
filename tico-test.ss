(import (micascheme) (tico))

(writeln (syntax->datum (parse-expr (empty-context) #`dupa)))
