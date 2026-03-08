(library (eof)
  (export eof eof?)
  (import (scheme))

  (define eof (eof-object))
  (define eof? eof-object?)
)
