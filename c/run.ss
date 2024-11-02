(library (c run)
  (export c-run)
  (import (micascheme))

  (define (c-run $string)
    (let ()
      (call-with-output-file "/tmp/main.c"
        (lambda ($port)
          (put-string $port $string))
        `(replace))
      (system "gcc /tmp/main.c -o /tmp/main")
      (system "/tmp/main")))
)
