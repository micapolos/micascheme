(library (typed keywords)
  (export typeof : assume assume-type expect)
  (import (micascheme))

  (define-aux-keywords typeof : assume assume-type expect)
)
