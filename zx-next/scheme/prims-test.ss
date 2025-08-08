(import
  (zx-next scheme test)
  (zx-next scheme prims)
  (zx-next tagged)
  (zx-next scheme tag)
  (zx-next scheme constant)
  (zx-next scheme value))

(define-values
  (offset #xa1)
  (value-1 (value #x56 #x1234))
  (value-2 (value #xbc #x789a))
  (pair-1 (pair-value pair-data-1)))

(define-fragments
  (hello-string (dz "hello"))
  (hello-world-string (dz "Hello, world!")))

(define-fragments
  (test-box (value-data (value #x56 #x1234)))
  (value-data-1 (value-data value-1))
  (value-data-2 (value-data value-2))
  (pair-data-1 (pair-data value-1 value-2)))

(test
  (case throw
    (assert-throws (throw)))

  (case void
    (ld d offset)
    (void)
    (assert de (offset/byte offset 0))
    (assert hl (constant-word void-constant)))

  (case unsafe-put-char
    (ld d offset)
    (load-value (char-value #\a))
    (unsafe-put-char)
    (assert de (offset/byte offset 0))
    (assert hl (constant-word void-constant))
    (writeln))

  (case put-char
    (ld d offset)
    (load-value (char-value #\a))
    (unsafe-put-char)
    (assert de (offset/byte offset 0))
    (assert hl (constant-word void-constant))
    (writeln))

  (case put-char/throws
    (ld d offset)
    (load-value (byte-value #x12))
    (assert-throws (put-char)))

  (case unsafe-put-string
    (ld d offset)
    (load-value (string-value hello-world-string))
    (unsafe-put-string)
    (assert de (offset/byte offset 0))
    (assert hl (constant-word void-constant))
    (writeln))

  (case put-string
    (ld d offset)
    (load-value (string-value hello-world-string))
    (unsafe-put-string)
    (assert de (offset/byte offset 0))
    (assert hl (constant-word void-constant))
    (writeln))

  (case put-string/throws
    (ld d offset)
    (load-value (byte-value #x12))
    (assert-throws (put-string)))

  (case null?
    (ld d offset)
    (load-value (null-value))
    (null?)
    (assert de (offset/byte offset #x00))
    (assert hl (constant-word true-constant)))

  (case not-null?
    (ld d offset)
    (load-value (byte-value #x12))
    (null?)
    (assert de (offset/byte offset #x00))
    (assert hl (constant-word false-constant)))

  (case byte?
    (ld d offset)
    (load-value (byte-value #x12))
    (byte?)
    (assert de (offset/byte offset #x00))
    (assert hl (constant-word true-constant)))

  (case not-byte?
    (ld d offset)
    (load-value (word-value #x1234))
    (byte?)
    (assert de (offset/byte offset #x00))
    (assert hl (constant-word false-constant)))

  (case word?
    (ld d offset)
    (load-value (word-value #x1234))
    (word?)
    (assert de (offset/byte offset #x00))
    (assert hl (constant-word true-constant)))

  (case not-word?
    (ld d offset)
    (load-value (byte-value #x12))
    (word?)
    (assert de (offset/byte offset #x00))
    (assert hl (constant-word false-constant)))

  (case pair?
    (ld d offset)
    (load-value (pair-value pair-data-1))
    (pair?)
    (assert de (offset/byte offset #x00))
    (assert hl (constant-word true-constant)))

  (case not-pair?
    (ld d offset)
    (load-value (byte-value #x12))
    (pair?)
    (assert de (offset/byte offset #x00))
    (assert hl (constant-word false-constant)))

  (case char?
    (ld d offset)
    (load-value (char-value #\A))
    (char?)
    (assert de (offset/byte offset #x00))
    (assert hl (constant-word true-constant)))

  (case not-char?
    (ld d offset)
    (load-value (byte-value #x12))
    (char?)
    (assert de (offset/byte offset #x00))
    (assert hl (constant-word false-constant)))

  (case string?
    (ld d offset)
    (load-value (string-value hello-world-string))
    (string?)
    (assert de (offset/byte offset #x00))
    (assert hl (constant-word true-constant)))

  (case not-string?
    (ld d offset)
    (load-value (symbol-value hello-string))
    (string?)
    (assert de (offset/byte offset #x00))
    (assert hl (constant-word false-constant)))

  (case symbol?
    (ld d offset)
    (load-value (symbol-value hello-string))
    (symbol?)
    (assert de (offset/byte offset #x00))
    (assert hl (constant-word true-constant)))

  (case not-symbol?
    (ld d offset)
    (load-value (string-value hello-world-string))
    (symbol?)
    (assert de (offset/byte offset #x00))
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
    (ld d offset)
    (load-value (pair-value pair-data-1))
    (car)
    (assert de (offset/byte #xa1 #x56))
    (assert hl #x1234))

  (case cdr
    (ld d offset)
    (load-value (pair-value pair-data-1))
    (cdr)
    (assert de (offset/byte #xa1 #xbc))
    (assert hl #x789a))

  (case car/throws
    (ld d offset)
    (load-value (byte-value #x12))
    (assert-throws (car)))

  (case cdr/throws
    (ld d offset)
    (load-value (byte-value #x12))
    (assert-throws (cdr)))

  ; (case cons
  ;   (ld d offset)
  ;   (load-value (string-value hello-world-string))
  ;   (push-top)
  ;   (ld d 0)
  ;   (load-value (symbol-value hello-string))
  ;   (cons)
  ;   (assert e #x01)
  ;   (assert hl #xe003)
  ;   (assert-byte (#xe003) #xde)
  ;   (assert-word (#xe004) #x0abc)
  ;   (assert-byte (#xe006) #x67)
  ;   (assert-word (#xe007) #x2345))

  (case if/true
    (ld d offset)
    (load-value (true-value))
    (if
      (load-value (byte-value #x12))
      (load-value (byte-value #x34)))
    (assert de (offset/byte offset #x12))
    (assert hl (constant-word byte-constant)))

  (case if/false
    (ld d offset)
    (load-value (false-value))
    (if
      (load-value (byte-value #x12))
      (load-value (byte-value #x34)))
    (assert de (offset/byte offset #x34))
    (assert hl (constant-word byte-constant)))

  ;(call wait-space)
)
