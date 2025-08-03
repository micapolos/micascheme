(import
  (zx-next core)
  (zx-next terminal)
  (zx-next write)
  (zx-next debug)
  (zx-next throw))

(run
  (call terminal-init)

  (writeln "=== Trying no-throw flow ===")
  (writeln "Entering catch...")
  (catch
    (writeln "Inside catch.")
    (writeln "Exiting catch..."))
  (if nc
    (then (writeln-ok "Did not throw."))
    (else (writeln-error "Did throw.")))
  (writeln)

  (writeln "=== Trying throw flow ===")
  (writeln "Entering  catch...")
  (catch
    (writeln "Inside catch.")
    (writeln "Throw!!!")
    (throw)
    (writeln-error "Should do be there after throw."))
  (if nc
    (then (writeln-error "Did not throw."))
    (else (writeln-ok "Did throw.")))
  (writeln)

  (call terminal-wait-space)
  (rst 0))
