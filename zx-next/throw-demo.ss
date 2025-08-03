(import
  (zx-next core)
  (zx-next terminal)
  (zx-next write)
  (zx-next debug)
  (zx-next throw))

(run
  (call terminal-init)

  (writeln "=== Trying no-throw ===")
  (writeln "Entering catch...")
  (catch
    (writeln "Inside catch.")
    (writeln "Exiting catch..."))
  (if nc
    (then (writeln-ok "Did not throw."))
    (else (writeln-error "Did throw.")))
  (writeln)

  (writeln "=== Trying throw ===")
  (writeln "Entering catch...")
  (catch
    (writeln "Inside catch.")
    (writeln "Throw!!!")
    (throw)
    (writeln-error "Should do be there after throw."))
  (if nc
    (then (writeln-error "Did not throw."))
    (else (writeln-ok "Did throw.")))
  (writeln)

  (writeln "=== Trying nested throw without re-throw ===")
  (writeln "Entering outer catch...")
  (catch
    (writeln "Inside outer catch.")
    (writeln "Entering inner catch...")
    (catch
      (writeln "Inside inner catch.")
      (writeln "Inner throw!!!")
      (throw)
      (writeln-error "Should do be there after inner throw.")))
  (if nc
    (then (writeln-ok "Did not throw."))
    (else (writeln-error "Did throw.")))
  (writeln)

  (writeln "=== Trying nested throw with re-throw ===")
  (writeln "Entering outer catch...")
  (catch
    (writeln "Inside outer catch.")
    (writeln "Entering inner catch...")
    (catch
      (writeln "Inside inner catch.")
      (writeln "Inner throw!!!")
      (throw)
      (writeln-error "Should do be there after inner throw."))
    (writeln "Outer throw!!!")
    (throw)
    (writeln-error "Should do be there after outer throw."))
  (if nc
    (then (writeln-error "Did not throw."))
    (else (writeln-ok "Did throw.")))
  (writeln)

  (call terminal-wait-space)
  (rst 0))
