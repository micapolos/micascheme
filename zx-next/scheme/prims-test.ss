(import
  (zx-next scheme test)
  (zx-next scheme prims)
  (zx-next tagged)
  (zx-next scheme tag)
  (zx-next scheme constant)
  (zx-next scheme value))

(define-values
  (offset-1 #xa1)
  (offset-2 #xa2)
  (value-1 (value offset-1 #x56 #x1234))
  (value-2 (value offset-2 #xbc #x789a))
  (pair-1 (pair-value offset-1 pair-data-1)))

(define-fragments
  (hello-string (dz "hello"))
  (hello-world-string (dz "Hello, world!")))

(define-fragments
  (test-box (value-data (value offset-1 #x56 #x1234)))
  (value-data-1 (value-data value-1))
  (value-data-2 (value-data value-2))
  (pair-data-1 (pair-data value-1 value-2)))

(test
  (case throw
    (assert-throws (throw)))

  (case void
    (ld d offset-1)
    (void)
    (assert de (offset/byte offset-1 0))
    (assert hl (constant-word void-constant)))

  (case unsafe-put-char
    (load-value (char-value offset-1 #\a))
    (unsafe-put-char)
    (assert de (offset/byte offset-1 0))
    (assert hl (constant-word void-constant))
    (writeln))

  (case put-char
    (load-value (char-value offset-1 #\a))
    (unsafe-put-char)
    (assert de (offset/byte offset-1 0))
    (assert hl (constant-word void-constant))
    (writeln))

  (case put-char/throws
    (load-value (byte-value offset-1 #x12))
    (assert-throws (put-char)))

  (case unsafe-put-string
    (load-value (string-value offset-1 hello-world-string))
    (unsafe-put-string)
    (assert de (offset/byte offset-1 0))
    (assert hl (constant-word void-constant))
    (writeln))

  (case put-string
    (load-value (string-value offset-1 hello-world-string))
    (unsafe-put-string)
    (assert de (offset/byte offset-1 0))
    (assert hl (constant-word void-constant))
    (writeln))

  (case put-string/throws
    (load-value (byte-value offset-1 #x12))
    (assert-throws (put-string)))

  (case null?
    (load-value (null-value offset-1))
    (null?)
    (assert de (offset/byte offset-1 #x00))
    (assert hl (constant-word true-constant)))

  (case not-null?
    (load-value (byte-value offset-1 #x12))
    (null?)
    (assert de (offset/byte offset-1 #x00))
    (assert hl (constant-word false-constant)))

  (case byte?
    (load-value (byte-value offset-1 #x12))
    (byte?)
    (assert de (offset/byte offset-1 #x00))
    (assert hl (constant-word true-constant)))

  (case not-byte?
    (load-value (word-value offset-1 #x1234))
    (byte?)
    (assert de (offset/byte offset-1 #x00))
    (assert hl (constant-word false-constant)))

  (case word?
    (load-value (word-value offset-1 #x1234))
    (word?)
    (assert de (offset/byte offset-1 #x00))
    (assert hl (constant-word true-constant)))

  (case not-word?
    (load-value (byte-value offset-1 #x12))
    (word?)
    (assert de (offset/byte offset-1 #x00))
    (assert hl (constant-word false-constant)))

  (case pair?
    (load-value (pair-value offset-1 pair-data-1))
    (pair?)
    (assert de (offset/byte offset-1 #x00))
    (assert hl (constant-word true-constant)))

  (case not-pair?
    (load-value (byte-value offset-1 #x12))
    (pair?)
    (assert de (offset/byte offset-1 #x00))
    (assert hl (constant-word false-constant)))

  (case char?
    (load-value (char-value offset-1 #\A))
    (char?)
    (assert de (offset/byte offset-1 #x00))
    (assert hl (constant-word true-constant)))

  (case not-char?
    (load-value (byte-value offset-1 #x12))
    (char?)
    (assert de (offset/byte offset-1 #x00))
    (assert hl (constant-word false-constant)))

  (case string?
    (load-value (string-value offset-1 hello-world-string))
    (string?)
    (assert de (offset/byte offset-1 #x00))
    (assert hl (constant-word true-constant)))

  (case not-string?
    (load-value (symbol-value offset-1 hello-string))
    (string?)
    (assert de (offset/byte offset-1 #x00))
    (assert hl (constant-word false-constant)))

  (case symbol?
    (load-value (symbol-value offset-1 hello-string))
    (symbol?)
    (assert de (offset/byte offset-1 #x00))
    (assert hl (constant-word true-constant)))

  (case not-symbol?
    (load-value (string-value offset-1 hello-world-string))
    (symbol?)
    (assert de (offset/byte offset-1 #x00))
    (assert hl (constant-word false-constant)))

  (case unsafe-unbox
    (ld de (offset/byte #x04 #x00))
    (ld hl test-box)
    (unsafe-unbox)
    (assert de #x0456)
    (assert hl #x1234))

  (case unsafe-car
    (ld de (offset/byte #x04 #x00))
    (ld hl pair-data-1)
    (unsafe-car)
    (assert de #x0456)
    (assert hl #x1234))

  (case unsafe-cdr
    (ld de (offset/byte #x04 #x00))
    (ld hl pair-data-1)
    (unsafe-cdr)
    (assert de #x04bc)
    (assert hl #x789a))

  (case car
    (load-value (pair-value offset-1 pair-data-1))
    (car)
    (assert de (offset/byte #xa1 #x56))
    (assert hl #x1234))

  (case cdr
    (load-value (pair-value offset-1 pair-data-1))
    (cdr)
    (assert de (offset/byte #xa1 #xbc))
    (assert hl #x789a))

  (case car/throws
    (load-value (byte-value offset-1 #x12))
    (assert-throws (car)))

  (case cdr/throws
    (load-value (byte-value offset-1 #x12))
    (assert-throws (cdr)))

  (case cons
    ; Load cdr
    (ld de #x0123)
    (ld hl #x4567)

    ; Push cdr
    (push de)
    (push hl)

    ; Load car
    (ld de #x890a)
    (ld hl #xbcde)

    ; Make cons
    (cons)

    (assert e #x01)
    (assert hl #xe003)
    (assert-byte (#xe003) #xde)
    (assert-word (#xe004) #x0abc)
    (assert-byte (#xe006) #x67)
    (assert-word (#xe007) #x2345))

  (case unsafe-invoke-0
    (load-value (procedure-value offset-1 void-proc))
    (unsafe-invoke)
    (assert de (offset/byte offset-1 0))
    (assert hl (constant-word void-constant)))

  (case unsafe-invoke-1
    (push-value (string-value offset-1 hello-world-string))
    (load-value (procedure-value 0 unsafe-put-string-proc))
    (unsafe-invoke)
    (assert de (offset/byte offset-1 0))
    (assert hl (constant-word void-constant)))

  ;(call wait-space)
)
