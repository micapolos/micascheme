(library (zx-next compiler expr-stacked)
  (export)
  (import
    (micascheme))

  (define (indexed->stacked $indexed)
    (syntax-case $indexed ()
      ((1 op (1 a) (1 b))
        #`(begin
          #,(indexed->stacked #'b)
          #,(indexed->stacked #'a)
          (%op 1 1 1 op)))))
)
