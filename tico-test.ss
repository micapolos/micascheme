(import (micascheme) (tico))

(writeln
  (syntax->datum
    (parse-expr
      (context-push (empty-context)
        (expr #`"my string" #`my-string))
      #`my-string)))
