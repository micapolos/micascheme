(library (case)
  (export case1)
  (import (scheme) (syntax))

  (define-syntax case1
    (syntax-rules (else)
      ((_ $expr ($key $body ...) ... ((else $var) $else-body ...))
        (let (($var $expr))
          (case $var (($key) $body ...) ... (else $else-body ...))))
      ((_ $expr ($key $body ...) ...)
        (case $expr (($key) $body ...) ...))))
)
