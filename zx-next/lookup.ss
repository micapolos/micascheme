(library (zx-next lookup)
  (export lookup-byte lookup-word lookup)
  (import (zx-next core))

  (define-ops (keywords a de)
    ((lookup a)
      (add hl a)
      (ld a (hl)))
    ((lookup de)
      (ld d 0)
      (ld e a)
      (add hl de)
      (add hl de)

      (ld e (hl))
      (inc hl)
      (ld d (hl))))

  (define-fragment lookup-byte
    (input (a index) (hl table))
    (output (a value))
    (lookup a)
    (ret))

  (define-fragment lookup-word
    (input (a index) (hl table))
    (output (de value))
    (lookup de)
    (ret))
)
