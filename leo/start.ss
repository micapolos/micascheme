(library (leo start)
  (export start)
  (import
    (micascheme)
    (leo leo)
    (leo version)
    (leo exception-handler)
    (prefix (leo scheme) %))

  (define (start $arguments)
    (syntax-case $arguments ()
      (() (start-help $arguments))
      (_ (start-options $arguments))))

  (define (start-options $arguments)
    (syntax-case $arguments ()
      (("-v" . x)
        (start-version (datum x)))
      (("--version" . x)
        (start-version (datum x)))
      (("-h" . x)
        (start-help (datum x)))
      (("--help" . x)
        (start-help (datum x)))
      ((file arg ...)
        (start-file (datum file) (datum (arg ...))))
      (()
        (void))))

  (define (start-version $arguments)
    (run
      (displayln (string-append "Leo Scheme " version))
      (start-options $arguments)))

  (define (start-help $arguments)
    (run
      (display
        (lines-string
          "usage: leo [options] [file [args]]"
          ""
          "Available options are:"
          "  -v, --version  show version information"
          "  -h, --help     show this help message"))
      (start-options $arguments)))

  (define (start-file $file $arguments)
    (parameterize
      ((command-line-arguments $arguments))
      (leo
        (with-exception-handler
          leo-exception-handler
          (lambda () (load $file))))))
)
