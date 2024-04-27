(library (masm bits)
  (export)
  (import
    (only (micascheme) data enum))

  (data (bits n))

  (enum
    (instr
      (push bits value)
      (drop bits)

      (inc bits)
      (add bits)
      (select bits)

      (dup bits)

      (var bits instr)
      (load bits offset)
      (store bits offset)

      (block instrs)
      (switch bits instrs)))
)
