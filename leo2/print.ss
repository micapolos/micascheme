(library (leo2 print)
  (export print)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 evaluate))

  (define (print $term)
    (pretty-print
      (native-ref
        (evaluated-ref
          (evaluate $term)))))
)
