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
        body ...
        (%writeln-ok "All tests passed."))))
)
