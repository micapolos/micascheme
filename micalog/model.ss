(library (micalog model)
  (export
    expr-domain expr-term expr-size)
  (import (micascheme))

  (define (expr-domain $expr)
    (syntax-case $expr ()
      ((domain _ _) #'domain)))

  (define (expr-size $expr)
    (syntax-case $expr ()
      ((_ size _) (datum size))))

  (define (expr-term $expr)
    (syntax-case $expr ()
      ((_ _ term) #'term)))
)

; (micalog
;   (wire 1 clock native)
;   (wire 1 reset? native)
;   (wire 16 mouse-x native)
;   (wire 16 mouse-y native)
;   (wire 1 mouse-pressed? native)
;   (on clock
;     (posedge
;       (reg 1 half-clock ?)
;       (reg 8 counter ?)
;       (wire 8 counter+16 (+ 8 counter 16))
;       (on half-clock
;         (posedge
;           (set! 8 counter next-counter))
;         (negedge
;           (cond
;             (reset? (set! 8 next-counter (ref 8 mouse-x (7 to 0))))
;             (mouse-pressed? (set! 8 next-counter (+ 8 counter 1)))
;             (else (set! 8 next-counter (- 8 counter 1)))))))
;     (negedge
;       (reg 1 next-half-clock 0)
;       (reg 8 next-counter 0)
;       (set! 1 next-half-clock (not 1 half-clock)))))
