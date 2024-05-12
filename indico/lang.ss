(library (indico lang)
  (export indico native)
  (import
    (scheme)
    (syntax)
    (list)
    (stack)
    (indico keywords)
    (indico expr))

  (export (import (indico keywords)))

  (define-aux-keywords native)

  (define-case-syntax (indico body ...)
    #`(values->list
      #,(expr-syntax
        (list-syntax->expr
          (lambda ($recurse $locals $syntax)
            (syntax-case $syntax (native)
              ((native body ...)
                (expr
                  (value-type (length (datum (body ...))))
                  #'(values body ...)))))
          (stack)
          #'(body ...)))))
)
