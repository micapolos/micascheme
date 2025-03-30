(library (typed-scheme keywords)
  (export assume expect any-lambda oneof forall)
  (import (micascheme))

  (define-aux-keywords assume expect any-lambda oneof forall)
)
