(library (typed-scheme keywords)
  (export a-lambda oneof forall a-boolean a-string a-number)
  (import (micascheme))

  (define-aux-keywords a-lambda oneof forall a-boolean a-string a-number)
)
