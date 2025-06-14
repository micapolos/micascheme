(import (micascheme) (simplang expander))

(parameterize
  ((current-expand
    (lambda ($syntax . $rest)
      (apply sc-expand (cdr (typed '() $syntax)) $rest))))
  (pretty-print (eval '(+ "foo" "bar")))
  (pretty-print (eval '(length (+ "foo" "bar")))))
