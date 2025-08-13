(import (asm z80) (prefix (zx-next compiler stacked) %) (zx-next compiler stacked-asm))

; op 0
(check-stacked->asm
  (() (%op 0 (nop)))
  (() (nop)))

(check-stacked->asm
  ((a) (%op 0 (nop)))
  ((a) (nop)))

; op 1
(check-stacked->asm
  (() (%op 1 (ld a #x12)))
  ((a) (ld a #x12)))

(check-stacked->asm
  ((a) (%op 1 (ld a #x12)))
  ((a l) (ld l a) (ld a #x12)))

(check-stacked->asm
  ((a de) (%op 1 (ld a #x12)))
  ((a l) (push de) (ld l a) (ld a #x12)))

(check-stacked->asm
  ((hl) (%op 1 (ld a #x12)))
  ((a de) (ex de hl) (ld a #x12)))

(check-stacked->asm
  ((hl de) (%op 1 (ld a #x12)))
  ((a de) (push de) (ex de hl) (ld a #x12)))

(check-stacked->asm
  ((lde) (%op 1 (ld a #x12)))
  ((a ) (push lde) (ld a #x12)))

(check-stacked->asm
  ((hlde) (%op 1 (ld a #x12)))
  ((a ) (push hlde) (ld a #x12)))

; op 2
(check-stacked->asm
  (() (%op 2 (ld hl #x1234)))
  ((hl) (ld hl #x1234)))

(check-stacked->asm
  ((a) (%op 2 (ld hl #x1234)))
  ((hl) (push a) (ld hl #x1234)))

(check-stacked->asm
  ((a l) (%op 2 (ld hl #x1234)))
  ((hl) (ld h a) (push hl) (ld hl #x1234)))

(check-stacked->asm
  ((a de) (%op 2 (ld hl #x1234)))
  ((hl) (push de) (push a) (ld hl #x1234)))

(check-stacked->asm
  ((hl) (%op 2 (ld hl #x1234)))
  ((hl de) (ex de hl) (ld hl #x1234)))

(check-stacked->asm
  ((hl de) (%op 2 (ld hl #x1234)))
  ((hl de) (push de) (ex de hl) (ld hl #x1234)))

(check-stacked->asm
  ((lde) (%op 2 (ld hl #x1234)))
  ((hl) (push lde) (ld hl #x1234)))

(check-stacked->asm
  ((hlde) (%op 2 (ld hl #x1234)))
  ((hl) (push hlde) (ld hl #x1234)))

; op 3
(check-stacked->asm
  (() (%op 3 (ld lde #x123456)))
  ((lde) (ld lde #x123456)))

(check-stacked->asm
  ((a) (%op 3 (ld lde #x123456)))
  ((lde) (push a) (ld lde #x123456)))

(check-stacked->asm
  ((a l) (%op 3 (ld lde #x123456)))
  ((lde) (ld h a) (push hl) (ld lde #x123456)))

(check-stacked->asm
  ((hl) (%op 3 (ld lde #x123456)))
  ((lde) (push hl) (ld lde #x123456)))

(check-stacked->asm
  ((hl de) (%op 3 (ld lde #x123456)))
  ((lde) (push de) (push hl) (ld lde #x123456)))

(check-stacked->asm
  ((lde) (%op 3 (ld lde #x123456)))
  ((lde) (push lde) (ld lde #x123456)))

(check-stacked->asm
  ((hlde) (%op 3 (ld lde #x123456)))
  ((hlde) (push hlde) (ld lde #x123456)))

; op 4
(check-stacked->asm
  (() (%op 4 (ld hlde #x12345678)))
  ((hlde) (ld hlde #x12345678)))

(check-stacked->asm
  ((a) (%op 4 (ld hlde #x12345678)))
  ((hlde) (push a) (ld hlde #x12345678)))

(check-stacked->asm
  ((a l) (%op 4 (ld hlde #x12345678)))
  ((hlde) (ld h a) (push hl) (ld hlde #x12345678)))

(check-stacked->asm
  ((hl) (%op 4 (ld hlde #x12345678)))
  ((hlde) (push hl) (ld hlde #x12345678)))

(check-stacked->asm
  ((hl de) (%op 4 (ld hlde #x12345678)))
  ((hlde) (push de) (push hl) (ld hlde #x12345678)))

(check-stacked->asm
  ((lde) (%op 4 (ld hlde #x12345678)))
  ((hlde) (push lde) (ld hlde #x12345678)))

(check-stacked->asm
  ((hlde) (%op 4 (ld hlde #x12345678)))
  ((hlde) (push hlde) (ld hlde #x12345678)))

; op 1 1
(check-stacked->asm
  (() (%op 1 1 (inc a)))
  ((a) (pop a) (inc a)))

(check-stacked->asm
  ((a) (%op 1 1 (inc a)))
  ((a) (inc a)))

(check-stacked->asm
  ((a l) (%op 1 1 (inc a)))
  ((a l) (inc a)))

(check-stacked->asm
  ((a de) (%op 1 1 (inc a)))
  ((a de) (inc a)))

; op 1 1 1
(check-stacked->asm
  (() (%op 1 1 1 (add l)))
  ((a) (pop hl) (ld a l) (ld l h) (add l)))

(check-stacked->asm
  ((a) (%op 1 1 1 (add l)))
  ((a) (pop l) (add l)))

(check-stacked->asm
  ((a l) (%op 1 1 1 (add l)))
  ((a) (add l)))

; append
(check-stacked->asm
  ((hl)
    (%op 1 (ld a #x12))
    (%op 1 (ld a #x13))
    (%op 1 1 1 (add a l))
    (%op 1 0 (write a)))
  (()
    (ex de hl)
    (ld a #x12)
    (push de)
    (ld l a)
    (ld a #x13)
    (add a l)
    (write a)))
