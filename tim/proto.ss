(library (tim proto)
  (export)
  (import (micascheme))

  (data (index-type bits))
  (data (array-type item bits))
  (data (tuple-type items))
  (data (ref-type type))
  (data (function-type inputs outputs))

  (define-aux-keyword local)

  (define (parse $vars $vals $syntax)
    (syntax-case $syntax (local)
      ((push $value)
        (u8? (datum $value))
        (values (push $vals (cons (datum $value)) )
      ((local $instr ...)
        (fold-left
          (partial ))
        (parse $vars (stack) $syntax)
        )))

  (define (u8? $x)
    (and
      (nonnegative-integer? $x)
      (<= $x #xff)))
)
