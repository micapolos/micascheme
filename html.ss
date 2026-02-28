(library (html)
  (export put-html html-string)
  (import (micascheme))

  (define-rule-syntax (html-string html)
    (call-with-string-output-port
      (lambda ($port)
        (put-string $port "<!DOCTYPE html>")
        (put-html $port html))))

  (define-rules-syntax (keywords with)
    ((put-html $port s)
      (string? (datum s))
      (put-string $port s))
    ((put-html $port (tag (with (name value) ...)))
      (begin
        (put-string $port "<")
        (put-string $port (symbol->string 'tag))
        (begin
          (put-string $port " ")
          (put-string $port (symbol->string 'name))
          (put-string $port "=")
          (put-string $port (format "~s" value)))
        ...
        (put-string $port "/>")))
    ((put-html $port (tag (with (name value) ...) child ...))
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
        (put-html $port child) ...
        (put-string $port "</")
        (put-string $port (symbol->string 'tag))
        (put-string $port ">")))
    ((put-html $port (tag child ...))
      (put-html $port (tag (with) child ...))))
)
