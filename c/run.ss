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
            (format "gcc /tmp/main.c -o /tmp/main~a -Wno-parentheses-equality"
              $libs-string))
          (run
            (displayln $string)
            (call-with-output-file "/tmp/main.c"
              (lambda ($port)
                (put-string $port $string))
              `(replace))
            (displayln $gcc-command)
            (system $gcc-command)
            (system "rm /tmp/main.c")
            (set! $value (system "/tmp/main"))
            (system "rm /tmp/main")
            $value)))))
)
