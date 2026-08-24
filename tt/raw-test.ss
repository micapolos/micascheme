(import
  (scheme)
  (check)
  (tt raw)
  (prefix (tt term) %)
  (prefix (tt primitive) %)
  (prefix (tt type) %))

(check-elaborated
  (lookup)
  (elaborated "type" "value")
  (elaborated "type" "value"))

(check-elaborated
  (lookup)
  (kind 0)
  (elaborated (%kind 1) (%kind 0)))

(check-elaborated
  (lookup (foo (elaborated "string" "foo")))
  (variable 'foo)
  (elaborated "string" "foo"))

(check-elaborated
  (lookup)
  (abstraction (variable 'x) "string" (variable 'x))
  (elaborated
    (%product "string" (lambda ($0) "string"))
    (%abstraction (lambda ($0) $0))))

(check-elaborated
  (lookup)
  (abstraction (variable 'x) "string"
    (abstraction (variable 'y) "number"
      (variable 'x)))
  (elaborated
    (%product "string"
      (lambda ($0)
        (%product "number"
          (lambda ($0) "string"))))
    (%abstraction
      (lambda ($0)
        (%abstraction
          (lambda ($1) $0))))))
