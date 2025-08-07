(library (zx-next scheme write)
  (export
    write-value
    write-stack)
  (import
    (zx-next core)
    (except (zx-next write) write-byte-literal write-word-literal)
    (zx-next scheme tag)
    (zx-next tag)
    (zx-next dispatch))

  (define-fragments
    (tag-string (dz "tag"))
    (constant-string (dz "constant"))
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

  (define-proc (write-unknown-tag a)
    (preserve (af)
      (call write-hash)
      (ld a #\<)
      (call write-char)
      (ld hl tag-string)
      (call write-string)
      (ld a #\-)
      (call write-char))
    (call write-nibble)
    (ld a #\>)
    (jp write-char))

  (define-proc (write-unknown-constant a)
    (preserve (af)
      (call write-hash)
      (ld a #\<)
      (call write-char)
      (ld hl constant-string)
      (call write-string)
      (ld a #\-)
      (call write-char))
    (call write-byte)
    (ld a #\>)
    (jp write-char))

  (define-proc (write-value de hl)
    (ld bc hl)
    (ld a b)
    (and tag-mask)
    (dup 3 (rlca))
    (dispatch
      (write-unknown-tag #x00)
      (write-unknown-tag #x01)
      (write-unknown-tag #x01)
      (begin
        (ld a b)
        (and #x1f)
        (dispatch
          (call write-null)
          (write "#<void>")
          (begin (ld a e) (call write-byte-literal))
          (begin (ld h c) (ld l e) (call write-word-literal))
          (write-unknown-constant #x04)
          (write-unknown-constant #x05)
          (write-unknown-constant #x06)
          (write-unknown-constant #x07)
          (write-unknown-constant #x08)
          (write-unknown-constant #x09)
          (write-unknown-constant #x0a)
          (write-unknown-constant #x0b)
          (write-unknown-constant #x0c)
          (write-unknown-constant #x0d)
          (write-unknown-constant #x0e)
          (write-unknown-constant #x0f)
          (call write-true)
          (begin (ld h c) (ld l e) (call write-string-literal))
          (begin (ld h c) (ld l e) (call write-symbol))
          (begin (ld a e) (call write-char-literal))
          (write-unknown-constant #x14)
          (write-unknown-constant #x15)
          (write-unknown-constant #x16)
          (write-unknown-constant #x17)
          (call write-false)
          (write-unknown-constant #x19)
          (write-unknown-constant #x1a)
          (write-unknown-constant #x1b)
          (write-unknown-constant #x1c)
          (write-unknown-constant #x1d)
          (write-unknown-constant #x1e)
          (write-unknown-constant #x1f)))
      (write-unknown-tag #x04)
      (write-unknown-tag #x05)
      (write-unknown-tag #x06)
      (write-unknown-tag #x07))
    (write-ink normal-color)
    (ret))

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
