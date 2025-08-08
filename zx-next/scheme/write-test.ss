(import
  (zx-next scheme test)
  (zx-next scheme prims)
  (zx-next scheme write)
  (zx-next tagged)
  (zx-next scheme tag)
  (zx-next scheme value)
  (zx-next scheme constant))

(define-values
  (offset #xa1)
  (value-1 (byte-value #x12))
  (value-2 (byte-value #x34))
  (pair-1 (pair-value pair-data-1)))

(define-fragments
  (hello-string (dz "hello"))
  (hello-world-string (dz "Hello, world!")))

(define-fragments
  (value-data-1 (value-data value-1))
  (value-data-2 (value-data value-2))
  (pair-data-1 (pair-data value-1 value-2)))

(define-op (case-write label value)
  (case label
    (ld d offset)
    (load-value value)
    (write)
    (writeln)))

(test
  (case write
    (ld d offset)
    (load-value (byte-value #x12))
    (write)
    (assert de (offset/byte offset 0))
    (assert hl (constant-word void-constant))
    (writeln))

  (case-write write-void (void-value))
  (case-write write-null (null-value))
  (case-write write-false (false-value))
  (case-write write-true (true-value))
  (case-write write-byte (byte-value #x12))
  (case-write write-word (word-value #x1234))
  (case-write write-char (logging (char-value #\A)))
  (case-write write-symbol (symbol-value hello-string))
  (case-write write-string (string-value hello-string))
  (case-write write-pair (pair-value pair-data-1))

  ;(call wait-space)
)
