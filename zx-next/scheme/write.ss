(library (zx-next scheme write)
  (export
    write-value
    write-stack)
  (import
    (zx-next core)
    (except (zx-next write) write-byte-literal write-word-literal)
    (zx-next scheme tag)
    (zx-next scheme value))

  (define-fragments
    (stack-string (dz "stack"))
    (unknown-string (dz "unknown")))

  (define-values
    (normal-color   7)
    (symbol-color   3)
    (boolean-color  5)
    (char-color     5)
    (string-color   2)
    (paren-color    7)
    (number-color   6)
    (hash-color     5))

  (define-fragment write-open
    (write-ink paren-color)
    (ld a #\()
    (jp write-char))

  (define-fragment write-close
    (write-ink paren-color)
    (ld a #\))
    (call write-char)
    (write-ink normal-color)
    (ret))

  (define-fragment write-quotes
    (write-ink string-color)
    (ld a #\")
    (jp write-char))

  (define-fragment write-hash
    (write-ink hash-color)
    (ld a #\#)
    (jp write-char))

  (define-fragment write-space
    (ld a #\space)
    (jp write-char))

  (define-fragment write-true
    (call write-hash)
    (write-ink boolean-color)
    (ld a #\t)
    (jp write-char))

  (define-fragment write-false
    (call write-hash)
    (write-ink boolean-color)
    (ld a #\f)
    (jp write-char))

  (define-fragment write-null
    (call write-open)
    (jp write-close))

  (define-fragment write-hex-prefix
    (call write-hash)
    (ld a #\x)
    (call write-char)
    (write-ink number-color)
    (ret))

  (define-fragment write-byte-literal
    (input (a byte))
    (preserve (af) (call write-hex-prefix))
    (jp write-byte))

  (define-fragment write-char-literal
    (input (a char))
    (preserve (af)
      (call write-hash)
      (ld a #\\)
      (call write-char)
      (write-ink char-color))
    (jp write-char))

  (define-fragment write-word-literal
    (input (hl word))
    (preserve (hl) (call write-hex-prefix))
    (jp write-word))

  (define-fragment write-symbol
    (input (hl addr))
    (preserve (hl) (write-ink symbol-color))
    (call write-string)
    (ret))

  (define-fragment write-string-literal
    (input (hl addr))
    (preserve (hl) (call write-quotes))
    (call write-string)
    (jp write-quotes))

  (define-fragment write-symbolic
    (input (bcd value) (hl symbol-addr))
    (preserve (bc de)
      (preserve (hl) (call write-open))
      (call write-symbol))
    (call write-value)
    (jp write-close))

  (define-fragment write-unknown
    (call write-hash)
    (ld a #\<)
    (call write-char)
    (ld hl unknown-string)
    (call write-symbol)
    (ld a #\>)
    (jp write-char))

  (define-fragment write-value
    (input (bcd value))
    (ld a b)
    (and #b11100000)

    (cp byte-tag)
    (when z
      (value->a)
      (jp write-byte-literal))

    (cp word-tag)
    (when z
      (value->hl)
      (jp write-word-literal))

    (cp char-tag)
    (when z
      (value->a)
      (jp write-char-literal))

    (cp constant-tag)
    (when z
      (ld a b)

      (cp null-tag)
      (when z (jp write-null))

      (cp false-tag)
      (when z (jp write-false))

      (cp true-tag)
      (when z (jp write-true))

      (jp write-unknown))

    (cp symbol-tag)
    (when z
      (value->mmu/hl)
      (jp write-symbol))

    (cp string-tag)
    (when z
      (value->mmu/hl)
      (jp write-string-literal))

    (jp write-unknown))

  (define-fragment write-stack
    (preserve (de)
      (preserve (de)
        (call write-open)
        (ld hl stack-string)
        (call write-symbol))

      (ld hl 4)
      (add hl sp)
      (ld a e)
      (add hl a)

      (loop
        ; Load value into bcde
        (ld e (hl))
        (inc hl)
        (ld d (hl))
        (inc hl)
        (ld c (hl))
        (inc hl)
        (ld b (hl))
        (inc hl)
        (ld a e)

        ; return if end of stack
        (cp #xff)
        (when z
          (call write-close)
          (call write-newline)
          (pop de)  ; compensate for (preserve (de)) - implement break from loop!!!
          (ret))

        ; advance to the next entry
        (add hl a)
        (preserve (hl)
          (preserve (bc de) (call write-space))
          (call write-value))))

    (ret))
)
