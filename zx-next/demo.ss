(library (zx-next demo)
  (export demo)
  (import
    (micascheme)
    (prefix (zx-next core) %)
    (prefix (zx-next terminal) %))
  (export
    (import
      (zx-next core)
      (zx-next terminal)
      (zx-next debug)
      (zx-next write)))

  (define-rule-syntax (demo body ...)
    (%run
      (%with-terminal
        body ...
        (%call %terminal-wait-space)
        (%jp 0))))
)
