(import (asm z80) (prefix (zx-next compiler stacked) %) (zx-next compiler stacked-asm))

; const 1
(check-stacked-asm
  (%stacked () (%const 1 #x12))
  (asm (a) (ld a #x12)))

(check-stacked-asm
  (%stacked (a) (%const 1 #x12))
  (asm (a l) (ld l a) (ld a #x12)))

(check-stacked-asm
  (%stacked (a de) (%const 1 #x12))
  (asm (a l) (push de) (ld l a) (ld a #x12)))

(check-stacked-asm
  (%stacked (hl) (%const 1 #x12))
  (asm (a de) (ex de hl) (ld a #x12)))

(check-stacked-asm
  (%stacked (hl de) (%const 1 #x12))
  (asm (a de) (push de) (ex de hl) (ld a #x12)))

(check-stacked-asm
  (%stacked (lde) (%const 1 #x12))
  (asm (a ) (push lde) (ld a #x12)))

(check-stacked-asm
  (%stacked (hlde) (%const 1 #x12))
  (asm (a ) (push hlde) (ld a #x12)))

; const 2
(check-stacked-asm
  (%stacked () (%const 2 #x1234))
  (asm (hl) (ld hl #x1234)))

(check-stacked-asm
  (%stacked (a) (%const 2 #x1234))
  (asm (hl) (push a) (ld hl #x1234)))

(check-stacked-asm
  (%stacked (a l) (%const 2 #x1234))
  (asm (hl) (ld h a) (push hl) (ld hl #x1234)))

(check-stacked-asm
  (%stacked (a de) (%const 2 #x1234))
  (asm (hl) (push de) (push a) (ld hl #x1234)))

(check-stacked-asm
  (%stacked (hl) (%const 2 #x1234))
  (asm (hl de) (ex de hl) (ld hl #x1234)))

(check-stacked-asm
  (%stacked (hl de) (%const 2 #x1234))
  (asm (hl de) (push de) (ex de hl) (ld hl #x1234)))

(check-stacked-asm
  (%stacked (lde) (%const 2 #x1234))
  (asm (hl) (push lde) (ld hl #x1234)))

(check-stacked-asm
  (%stacked (hlde) (%const 2 #x1234))
  (asm (hl) (push hlde) (ld hl #x1234)))

; inc 1
(check-stacked-asm
  (%stacked () (%op 1 1 (inc a)))
  (asm (a) (pop a) (inc a)))

(check-stacked-asm
  (%stacked (a) (%op 1 1 (inc a)))
  (asm (a) (inc a)))

(check-stacked-asm
  (%stacked (a l) (%op 1 1 (inc a)))
  (asm (a l) (inc a)))

(check-stacked-asm
  (%stacked (a de) (%op 1 1 (inc a)))
  (asm (a de) (inc a)))

; neg 1
(check-stacked-asm
  (%stacked (a) (%neg 1))
  (asm (a) (neg)))

; cpl 1
(check-stacked-asm
  (%stacked (a) (%cpl 1))
  (asm (a) (cpl)))

; add 1
(check-stacked-asm
  (%stacked () (%add 1))
  (asm (a) (pop hl) (ld a l) (ld l h) (add l)))

(check-stacked-asm
  (%stacked (a) (%add 1))
  (asm (a) (pop l) (add l)))

(check-stacked-asm
  (%stacked (a l) (%add 1))
  (asm (a) (add l)))

; peek 1
(check-stacked-asm
  (%stacked () (%peek 1))
  (asm (a) (pop hl) (ld a (hl))))

(check-stacked-asm
  (%stacked (hl) (%peek 1))
  (asm (a) (ld a (hl))))

; peek-offset 1
(check-stacked-asm
  (%stacked () (%peek 1 12))
  (asm (a) (ld a (+ ix 12))))

(check-stacked-asm
  (%stacked (a) (%peek 1 12))
  (asm (a l) (ld l a) (ld a (+ ix 12))))
