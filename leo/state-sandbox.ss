(import (micascheme) (syntax lookup) (leo state))

(define state
  (lookup-state
    (lookup-with
      (string-append (value-binding string-append))
      (string-length (value-binding string-length))
      (+ (value-binding +)))
    (lambda ($end-args)
      (for-each displayln (reverse $end-args)))))

(fluent state
  (state-push #'"foo")
  (state-push #'"bar")
  (state-begin #'string-append)
  (state-push #'"!")
  (state-end)
  (state-end))
