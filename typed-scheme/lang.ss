(library (typed-scheme lang)
  (export type)
  (import
    (micascheme)
    (typed-scheme type)
    (typed-scheme type-syntax)
    (typed-scheme keywords))
  (export (import (typed-scheme keywords)))

  (define-syntax (type $syntax)
    (syntax-case $syntax ()
      ((_ t)
        (syntax->type (stack) #'t))))
)
