(library (browser)
  (export browse)
  (import
    (micascheme)
    (html))

  (define-rules-syntax (keywords html)
    ((browse x)
      (lets
        ($path "/tmp/index.html")
        (run
          (call-with-port
            (open-output-file $path '(replace))
            (lambda ($port) (put-html $port x)))
          (system (format "open ~a" $path))))))
)
