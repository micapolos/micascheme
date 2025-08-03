(import
  (zx-next demo)
  (zx-next local))

(demo
  (local
    (ld hl #x1234)
    (push hl)
    (ld hl #x5678)
    (push hl)

    (write "Local -2: ")
    (ld-local de -2)
    (ex de hl)
    (call write-word)
    (writeln)

    (write "Local -4: ")
    (ld-local de -4)
    (ex de hl)
    (call write-word)
    (writeln)))
