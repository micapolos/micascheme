(library (zx-next scheme keywords)
  (export
    asm
    void begin quote lets throw
    void? null? boolean? byte? word? char? string? symbol? pair?
    box cons car cdr
    write
    put-char put-string)
  (import (only (micascheme) define-keywords))

  (define-keywords
    asm
    void begin quote lets throw
    void? null? boolean? byte? word? char? string? symbol? pair?
    box cons car cdr
    write
    put-char put-string)
)
