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

  (define-proc (write-value de hl)
    (ld bc hl)
    (ld a b)
    (and tag-mask)
    (dup 3 (rlca))
    (dispatch
      (write "#<tag-0>")
      (write "#<tag-1>")
      (write "#<tag-2>")
      (begin
        (ld a b)
        (and #x1f)
        (dispatch
          (call write-null)
          (write "#<void>")
          (begin (ld a e) (call write-byte-literal))
          (begin (ld h c) (ld l e) (call write-word-literal))
          (write "#<constant-04>")
          (write "#<constant-05>")
          (write "#<constant-06>")
          (write "#<constant-07>")
          (write "#<constant-08>")
          (write "#<constant-09>")
          (write "#<constant-0a>")
          (write "#<constant-0b>")
          (write "#<constant-0c>")
          (write "#<constant-0d>")
          (write "#<constant-0e>")
          (write "#<constant-0f>")
          (call write-true)
          (begin (ld h c) (ld l e) (call write-string-literal))
          (begin (ld h c) (ld l e) (call write-symbol))
          (begin (ld a e) (call write-char-literal))
          (write "#<constant-14>")
          (write "#<constant-15>")
          (write "#<constant-16>")
          (write "#<constant-17>")
          (call write-false)
          (write "#<constant-19>")
          (write "#<constant-1a>")
          (write "#<constant-1b>")
          (write "#<constant-1c>")
          (write "#<constant-1d>")
          (write "#<constant-1e>")
          (write "#<constant-1f>")))
      (write "#<tag-4>")
      (write "#<tag-5>")
      (write "#<tag-6>")
      (write "#<tag-7>"))
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
