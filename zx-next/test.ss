(library (zx-next test)
  (export test case)
  (import
    (except (micascheme) test case)
    (prefix (zx-next demo) %)
    (prefix (zx-next regs) %)
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
          (%then
            (%writeln-ok "All tests passed.")
            (%call %wait-a-sec)
            (%exit))
          (%else
            (%throw))))))

  (%define-op-syntax (case $syntax)
    (syntax-case $syntax ()
      ((_ label body ...)
        #`(%begin
          ; TODO: Make sure that all registers are preserved.
          (%preserve (%hl %af)
            (%write "testing ")
            (%write #,(symbol->string (datum label)))
            (%writeln "..."))
          body ...))))
)
