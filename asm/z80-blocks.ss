(library (asm z80-blocks)
  (export
    input output
    loop loop-djnz
    preserve
    if then else
    while)
  (import
    (asm lang) (asm z80))

  (define-keywords then else while)

  (define-ops (keywords then else while)
    ((input body ...))
    ((output body ...))
    ((loop body ... (while cond))
      (with-labels (label)
        label
        body ...
        (jp cond label)))
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
