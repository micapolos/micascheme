(library (asm-3 z80-blocks)
  (export
    input output
    loop loop-djnz
    if then else)
  (import
    (asm-3 lang) (asm-3 z80))

  (define-keywords then else)

  (define-ops (keywords then else)
    ((input body ...))
    ((output body ...))
    ((loop body ...) (with-tmp id id body ... (jp id)))
    ((loop-djnz body ...) (with-tmp id id body ... (djnz id)))
    ((if flag (then then-body ...) (else else-body ...))
      (with-tmp label-then
        (with-tmp label-end
          (jp flag label-then)
          else-body ...
          (jp label-end)
          label-then
          then-body ...
          label-end))))
)
