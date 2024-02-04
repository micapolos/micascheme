(library (zexy instr)
  (export
    reg-a reg-a?
    reg-f reg-f?
    reg-b reg-b?
    reg-c reg-c?
    reg-d reg-d?
    reg-e reg-e?
    reg-h reg-h?
    reg-l reg-l?

    op-ld-r-r op-ld-r-r? op-ld-r-r-lhs op-ld-r-r-rhs)
  (import (micascheme))

  (data (reg-a))
  (data (reg-f))

  (data (reg-b))
  (data (reg-c))
  (data (reg-d))
  (data (reg-e))
  (data (reg-h))
  (data (reg-l))

  (data (reg-bc))
  (data (reg-de))
  (data (reg-hl))
  (data (reg-sp))
  (data (reg-af))

  (enum (reg-8 reg-b reg-c reg-d reg-e reg-h reg-l reg-a))
  (enum (reg-16 bc de hl))

  (data (op-ld-r-r lhs rhs))
)
