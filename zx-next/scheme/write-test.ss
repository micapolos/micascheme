(import
  (zx-next scheme test)
  (zx-next scheme prims)
  (zx-next scheme write)
  (zx-next tagged)
  (zx-next scheme tag)
  (zx-next scheme constant))

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

(define-op (case-write label value)
  (case label
    (load-value value)
    (write)
    (writeln)))

(test
  (case write
    (load-value (byte-value offset-1 #x12))
    (write)
    (assert de (offset/byte offset-1 0))
    (assert hl void-constant-word)
    (writeln))

  (case-write write-void (void-value offset-1))
  (case-write write-null (null-value offset-1))
  (case-write write-false (false-value offset-1))
  (case-write write-true (true-value offset-1))
  (case-write write-byte (byte-value offset-1 #x12))
  (case-write write-word (word-value offset-1 #x1234))
  (case-write write-char (logging (char-value offset-1 #\A)))
  (case-write write-symbol (symbol-value offset-1 hello-string))
  (case-write write-string (string-value offset-1 hello-string))
  (case-write write-pair (pair-value offset-1 pair-data-1))

  (call wait-space)
)
