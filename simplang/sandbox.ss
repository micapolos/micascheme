(import (micascheme) (simplang expander))

(parameterize
  ((current-expand
    (lambda ($syntax . $rest)
      (apply sc-expand (cdr (typed std-scope $syntax)) $rest))))
  (pretty-print (eval '(string-append "foo" "bar")))
  (pretty-print (eval '(string-length (string-append "foo" "bar")))))
