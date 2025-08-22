(library (zx-next demo)
  (export demo)
  (import
    (micascheme)
    (prefix (zx-next core) %)
    (prefix (zx-next terminal) %)
    (prefix (zx-next debug) %)
    (prefix (zx-next write) %))
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
        (%writeln "\x10;\x6;Press \x10;\x2;SPACE\x10;\x6; to exit...\x10;\x7;")
        (%call %wait-space)
        (%exit))))
)
