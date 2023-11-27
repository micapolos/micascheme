(library (throw)
  (export throw or-throw)
  (import (scheme))

  (define-syntax throw
    (lambda (stx)
      (syntax-case stx ()
        ((_ name item ...)
          (identifier? #`name)
          #`(error #f (format "~s" (list (quote name) #,@(syntax->list #`(item ...)))))))))

  (define-syntax or-throw
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ ($target $arg ...))
          (identifier? #'$target)
          #'(or
            ($target $arg ...)
            (throw or-throw ($target $arg ...))))
        ((_ $other)
          #'(or $other
            (throw or-throw $other))))))
)
