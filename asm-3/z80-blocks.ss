(library (asm-3 z80-blocks)
  (export
    input output
    loop loop-djnz
    preserve
    if then else)
  (import
    (asm-3 lang) (asm-3 z80))

  (define-keywords then else)

  (define-ops (keywords then else)
    ((input body ...))
    ((output body ...))
    ((loop body ...)
      (with-labels (label)
        label
        body ...
        (jp label)))
    ((loop-djnz body ...)
      (with-labels (label)
        label
        body ...
        (djnz label)))
    ((preserve (reg ...) body ...)
      (push reg) ...
      body ...
      (reverse (pop reg) ...))
    ((if flag (then then-body ...) (else else-body ...))
      (with-labels (label-then label-end)
        (jp flag label-then)
        else-body ...
        (jp label-end)
        label-then
        then-body ...
        label-end)))
)
