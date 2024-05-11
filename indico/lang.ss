(library (indico lang)
  (export indico native block)
  (import
    (scheme)
    (syntax)
    (list)
    (stack)
    (indico keywords)
    (indico expr))

  (export (import (indico keywords)))

  (define-aux-keywords native block)

  (define-case-syntax (indico body ...)
    #`(values->list
      #,(expr-syntax
        (list-syntax->expr
          (lambda ($recurse $locals $syntax)
            (syntax-case $syntax (native)
              ((native body ...)
                (expr
                  (value-type (length (datum (body ...))))
                  #'(values body ...)))
              ((block (arg ...) body ...)
                ($recurse $locals
                  #`(
                    (call
                      (function
                        #,(expr-arity ($recurse $locals #'(arg ...)))
                        body ...)
                      arg ...))))))
          (stack)
          #'(body ...)))))
)
