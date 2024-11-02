(library (c run)
  (export c-run)
  (import (micascheme))

  (define c-run
    (case-lambda
      (($string)
        (c-run $string (list)))
      (($string $libs)
        (lets
          ($value 0)
          ($libs-string
            (apply string-append
              (map-with ($lib $libs) (string-append " -l" $lib))))
          ($gcc-command
            (format "gcc /tmp/main.c -o /tmp/main~a"
              $libs-string))
          (run
            (display $string)
            (echo "code-gen /tmp/main.c")
            (call-with-output-file "/tmp/main.c"
              (lambda ($port)
                (put-string $port $string))
              `(replace))
            (echo-system $gcc-command)
            (echo-system "rm /tmp/main.c")
            (set! $value (echo-system "/tmp/main"))
            (echo-system "rm /tmp/main")
            $value)))))

  (define (echo $string)
    (displayln (string-append "% " $string)))

  (define (echo-system $string)
    (run
      (echo $string)
      (system $string)))
)
