(library (c run)
  (export c-run c-run-echo?)
  (import (micascheme))

  (define c-run-echo? (make-parameter #f))

  (define (c-run $string . $gcc-flags)
    (lets
      ($value 0)
      ($gcc-flags-string
        (apply string-append
          (map-with
            ($gcc-flag $gcc-flags)
            (string-append " " $gcc-flag))))
      ($gcc-command
        (format "gcc /tmp/main.c -O~a~a -o /tmp/main"
          (optimize-level)
          $gcc-flags-string))
      (run
        (if (c-run-echo?) (display $string))
        (echo "code-gen /tmp/main.c")
        (call-with-output-file "/tmp/main.c"
          (lambda ($port)
            (put-string $port $string))
          `(replace))
        (echo-system $gcc-command)
        (echo-system "rm /tmp/main.c")
        (set! $value (echo-system "/tmp/main"))
        (echo-system "rm /tmp/main")
        $value)))

  (define (echo $string)
    (if (c-run-echo?) (displayln (string-append "% " $string))))

  (define (echo-system $string)
    (run
      (echo $string)
      (system $string)))
)
