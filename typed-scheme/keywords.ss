(library (typed-scheme keywords)
  (export assume expect a-lambda oneof forall)
  (import (micascheme))

  (define-aux-keywords assume expect a-lambda oneof forall)
)
