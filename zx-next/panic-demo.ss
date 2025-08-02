(import
  (zx-next core)
  (zx-next terminal)
  (zx-next write)
  (zx-next debug)
  (zx-next panic))

(run
  (call terminal-init)

  (writeln "=== Trying no-panic flow ===")
  (writeln "Entering panic handler...")
  (with-panic
    (writeln "  Entered panic handler.")
    (writeln "  No panic, please!")
    (writeln "  Exiting panic handler..."))
  (if nc
    (then (writeln "Did not panic."))
    (else (writeln "Did panic.")))
  (writeln)

  (writeln "=== Trying panic flow ===")
  (writeln "Entering panic handler...")
  (with-panic
    (writeln "  Entered panic handler.")
    (writeln "  Panic!!!")
    (panic)
    (writeln "  FIRE FIRE FIRE!!!")
    (writeln "  Exiting panic handler..."))
  (if nc
    (then (writeln "Did not panic."))
    (else (writeln "Did panic.")))
  (writeln)

  (call terminal-wait-space)
  (rst 0))
