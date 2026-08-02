(library (tt boolean)
  (export boolean=? and or)
  (import
    (tt lang)
    (prefix (tt lang-macros) %)
    (prefix (scheme) %))

  (define-macro and %compile-and)
  (define-macro or %compile-or)

  (define boolean=? (unchecked (pi (boolean boolean) boolean) %boolean=?))
  (define true? (unchecked (pi (boolean) boolean) (%lambda ($x) $x)))
  (define false? (unchecked (pi (boolean) boolean) (%lambda ($x) (%not $x))))
)
