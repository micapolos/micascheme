(library (zx-next compiler)
  (export
    (rename))
  (import (zx-next core))

  (define-rules-syntaxes
    ((inc r) '(inc r))
    ((dec r) '(dec r))
    ((add r) '(add r))
    ((preserve (rr ...) body ...)
      (begin
        (push rr) ...
        body ...
        (backwards (pop rr) ...))))

  (define-rules-syntax
    ((expression (byte-op1 op x))
      (begin
        (expression x)
        (op a)))
    ((expression (byte-op2 x y))
      (begin
        (expression y)
        (ld l a)
        (preserve (hl) (expression x))
        (op a l))))
)
