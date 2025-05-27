(library (typed-scheme keywords)
  (export
    assume expect any-lambda oneof forall
    in out)
  (import (micascheme))

  (define-keywords
    assume expect any-lambda oneof forall
    in out)
)
