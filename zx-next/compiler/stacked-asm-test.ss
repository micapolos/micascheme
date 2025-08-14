(import (asm z80) (zx-next compiler stacked-asm))

; not preserve-regs?
(check-stacked->asm
  (() (0 #f (nop)))
  (() (nop)))

(check-stacked->asm
  ((a) (0 #f (nop)))
  (() (push a) (nop)))

(check-stacked->asm
  ((a l) (0 #f (nop)))
  (() (ld h a) (push hl) (nop)))

(check-stacked->asm
  ((a de) (0 #f (nop)))
  (() (push de) (push a) (nop)))

(check-stacked->asm
  ((hl) (0 #f (nop)))
  (() (push hl) (nop)))

(check-stacked->asm
  ((hl de) (0 #f (nop)))
  (() (push de) (push hl) (nop)))

(check-stacked->asm
  ((lde) (0 #f (nop)))
  (() (push lde) (nop)))

(check-stacked->asm
  ((hlde) (0 #f (nop)))
  (() (push hlde) (nop)))

(check-stacked->asm
  ((a) (1 0 #f (nop)))
  (() (nop)))

(check-stacked->asm
  ((a l) (1 0 #f (nop)))
  (() (push l) (nop)))

(check-stacked->asm
  ((a de) (1 0 #f (nop)))
  (() (push de) (nop)))

(check-stacked->asm
  ((hl) (2 0 #f (nop)))
  (() (nop)))

(check-stacked->asm
  ((hl de) (2 0 #f (nop)))
  (() (push de) (nop)))

(check-stacked->asm
  ((lde) (3 0 #f (nop)))
  (() (nop)))

(check-stacked->asm
  ((hlde) (4 0 #f (nop)))
  (() (nop)))

; op 0
(check-stacked->asm
  (() (0 #t (nop)))
  (() (nop)))

(check-stacked->asm
  ((a) (0 #t (nop)))
  ((a) (nop)))

(check-stacked->asm
  ((a de) (0 #t (nop)))
  ((a de) (nop)))

; op 1
(check-stacked->asm
  (() (1 #t (ld a #x12)))
  ((a) (ld a #x12)))

(check-stacked->asm
  ((a de) (1 #f (ld a #x12)))
  ((a) (push de) (push a) (ld a #x12)))

(check-stacked->asm
  ((a) (1 #t (ld a #x12)))
  ((a l) (ld l a) (ld a #x12)))

(check-stacked->asm
  ((a de) (1 #t (ld a #x12)))
  ((a l) (push de) (ld l a) (ld a #x12)))

(check-stacked->asm
  ((hl) (1 #t (ld a #x12)))
  ((a de) (ex de hl) (ld a #x12)))

(check-stacked->asm
  ((hl de) (1 #t (ld a #x12)))
  ((a de) (push de) (ex de hl) (ld a #x12)))

(check-stacked->asm
  ((lde) (1 #t (ld a #x12)))
  ((a ) (push lde) (ld a #x12)))

(check-stacked->asm
  ((hlde) (1 #t (ld a #x12)))
  ((a ) (push hlde) (ld a #x12)))

; op 2
(check-stacked->asm
  (() (2 #t (ld hl #x1234)))
  ((hl) (ld hl #x1234)))

(check-stacked->asm
  ((a de) (2 #f (ld hl #x1234)))
  ((hl) (push de) (push a) (ld hl #x1234)))

(check-stacked->asm
  ((a) (2 #t (ld hl #x1234)))
  ((hl) (push a) (ld hl #x1234)))

(check-stacked->asm
  ((a l) (2 #t (ld hl #x1234)))
  ((hl) (ld h a) (push hl) (ld hl #x1234)))

(check-stacked->asm
  ((a de) (2 #t (ld hl #x1234)))
  ((hl) (push de) (push a) (ld hl #x1234)))

(check-stacked->asm
  ((hl) (2 #t (ld hl #x1234)))
  ((hl de) (ex de hl) (ld hl #x1234)))

(check-stacked->asm
  ((hl de) (2 #t (ld hl #x1234)))
  ((hl de) (push de) (ex de hl) (ld hl #x1234)))

(check-stacked->asm
  ((lde) (2 #t (ld hl #x1234)))
  ((hl) (push lde) (ld hl #x1234)))

(check-stacked->asm
  ((hlde) (2 #t (ld hl #x1234)))
  ((hl) (push hlde) (ld hl #x1234)))

; op 3
(check-stacked->asm
  (() (3 #t (ld lde #x123456)))
  ((lde) (ld lde #x123456)))

(check-stacked->asm
  ((a) (3 #t (ld lde #x123456)))
  ((lde) (push a) (ld lde #x123456)))

(check-stacked->asm
  ((a l) (3 #t (ld lde #x123456)))
  ((lde) (ld h a) (push hl) (ld lde #x123456)))

(check-stacked->asm
  ((hl) (3 #t (ld lde #x123456)))
  ((lde) (push hl) (ld lde #x123456)))

(check-stacked->asm
  ((hl de) (3 #t (ld lde #x123456)))
  ((lde) (push de) (push hl) (ld lde #x123456)))

(check-stacked->asm
  ((lde) (3 #t (ld lde #x123456)))
  ((lde) (push lde) (ld lde #x123456)))

(check-stacked->asm
  ((hlde) (3 #t (ld lde #x123456)))
  ((hlde) (push hlde) (ld lde #x123456)))

; op 4
(check-stacked->asm
  (() (4 #t (ld hlde #x12345678)))
  ((hlde) (ld hlde #x12345678)))

(check-stacked->asm
  ((a) (4 #t (ld hlde #x12345678)))
  ((hlde) (push a) (ld hlde #x12345678)))

(check-stacked->asm
  ((a l) (4 #t (ld hlde #x12345678)))
  ((hlde) (ld h a) (push hl) (ld hlde #x12345678)))

(check-stacked->asm
  ((hl) (4 #t (ld hlde #x12345678)))
  ((hlde) (push hl) (ld hlde #x12345678)))

(check-stacked->asm
  ((hl de) (4 #t (ld hlde #x12345678)))
  ((hlde) (push de) (push hl) (ld hlde #x12345678)))

(check-stacked->asm
  ((lde) (4 #t (ld hlde #x12345678)))
  ((hlde) (push lde) (ld hlde #x12345678)))

(check-stacked->asm
  ((hlde) (4 #t (ld hlde #x12345678)))
  ((hlde) (push hlde) (ld hlde #x12345678)))

; op 1 0
(check-stacked->asm
  (() (1 0 #t (write a)))
  (() (pop a) (write a)))

(check-stacked->asm
  ((a) (1 0 #t (write a)))
  (() (write a)))

(check-stacked->asm
  ((a l) (1 0 #t (write a)))
  ((l) (write a)))

(check-stacked->asm
  ((a de) (1 0 #t (write a)))
  ((hl) (write a) (ex de hl)))

; op 2 0
(check-stacked->asm
  (() (2 0 #t (write hl)))
  (() (pop hl) (write hl)))

(check-stacked->asm
  ((hl) (2 0 #t (write hl)))
  (() (write hl)))

(check-stacked->asm
  ((hl de) (2 0 #t (write hl)))
  ((hl) (write hl) (ex de hl)))

; op 3 0
(check-stacked->asm
  (() (3 0 #t (write lde)))
  (() (pop lde) (write lde)))

(check-stacked->asm
  ((lde) (3 0 #t (write lde)))
  (() (write lde)))

; op 4 0
(check-stacked->asm
  (() (4 0 #t (write hlde)))
  (() (pop hlde) (write hlde)))

(check-stacked->asm
  ((hlde) (4 0 #t (write hlde)))
  (() (write hlde)))

; op 1 1
(check-stacked->asm
  (() (1 1 #t (inc a)))
  ((a) (pop a) (inc a)))

(check-stacked->asm
  ((a) (1 1 #t (inc a)))
  ((a) (inc a)))

(check-stacked->asm
  ((a l) (1 1 #t (inc a)))
  ((a l) (inc a)))

(check-stacked->asm
  ((a de) (1 1 #t (inc a)))
  ((a de) (inc a)))

; op 1 1 1
(check-stacked->asm
  (() (1 1 1 #t (add l)))
  ((a) (pop hl) (ld a l) (ld l h) (add l)))

(check-stacked->asm
  ((a) (1 1 1 #t (add l)))
  ((a) (pop l) (add l)))

(check-stacked->asm
  ((a l) (1 1 1 #t (add l)))
  ((a) (add l)))

; append
(check-stacked->asm
  ((hl)
    (1 #t (ld a #x12))
    (1 #t (ld a #x13))
    (1 1 1 #t (add a l))
    (1 0 #t (write a)))
  (()
    (ex de hl)
    (ld a #x12)
    (push de)
    (ld l a)
    (ld a #x13)
    (add a l)
    (write a)))
