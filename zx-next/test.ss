(library (zx-next test)
  (export test)
  (import
    (except (micascheme) test)
    (prefix (zx-next demo) %)
    (prefix (zx-next throw) %))
  (export
    (import
      (zx-next core)
      (zx-next assert)
      (zx-next throw)))

  (define-rule-syntax (test body ...)
    (%demo
      (%catch
        (%catch body ...)
        (%if %nc
          (%then (%writeln-ok "All tests passed."))
          (%else (%throw))))))
)
