(library (asm z80-blocks)
  (export
    input output
    loop loop-djnz
    proc data
    preserve
    if then else)
  (import
    (asm lang)
    (asm std)
    (asm asm)
    (asm asm-core)
    (asm z80))

  (define-asm-rule (input body ...))

  (define-asm-rule (output body ...))

  (define-asm-rule (loop body ...)
    (local
      (label __loop)
      body ...
      (jp __loop)))

  (define-asm-rule (loop-djnz body ...)
    (local
      (label __loop)
      body ...
      (djnz __loop)))

  (define-asm-rule (proc id body ...)
    (label id)
    (local body ...))

  (define-asm-rule (data id body ...)
    (label id)
    (local body ...))

  (define-asm-rule (preserve (reg ...) body ...)
    (push reg) ...
    body ...
    (reverse (pop reg) ...))

  (define-keywords then else)

  (define-asm-rule (if flag (then then-body ...) (else else-body ...))
    (jp flag __then)
    else-body ...
    (jp __end)
    (label __then)
    then-body ...
    (label __end))
)
