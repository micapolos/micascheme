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
  (writeln "Exited panic handler.")
  (writeln)

  (writeln "=== Trying panic flow ===")
  (writeln "Entering panic handler...")
  (with-panic
    (writeln "  Entered panic handler.")
    (writeln "  Panic!!!")
    (panic)
    (writeln "  FIRE FIRE FIRE!!!")
    (writeln "  Exiting panic handler..."))
  (writeln "Exited panic handler.")
  (call terminal-wait-space)
  (rst 0))
