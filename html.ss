(library (html)
  (export html-put html-string)
  (import (micascheme))

  (define-rule-syntax (html-string html)
    (call-with-string-output-port
      (lambda ($port)
        (html-put $port html))))

  (define-rules-syntax (literals with)
    ((html-put $port s)
      (string? (datum s))
      (put-string $port s))
    ((html-put $port (tag (with (name value) ...) child ...))
      (begin
        (put-string $port "<")
        (put-string $port (symbol->string 'tag))
        (begin
          (put-string $port " ")
          (put-string $port (symbol->string 'name))
          (put-string $port "=")
          (put-string $port (format "~s" value)))
        ...
        (put-string $port ">")
        (html-put $port child) ...
        (put-string $port "</")
        (put-string $port (symbol->string 'tag))
        (put-string $port ">")))
    ((html-put $port (tag child ...))
      (html-put $port (tag (with) child ...))))
)
