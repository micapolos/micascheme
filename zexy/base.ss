(library (zexy base)
  (export)
  (import
    (except (micascheme) define)
    (only (scheme) define)
    (zexy math)
    (zexy unit))
  (export
    (import
      (except (micascheme) define)
      (only (scheme) define)
      (zexy math)
      (zexy unit))))
