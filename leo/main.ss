(import
  (leo scheme)
  (leo load)
  (leo version)
  (only (micascheme) lines-string))

(define (display-version)
  (displayln (string-append "Leo " version)))

(define (display-help)
  (display
    (lines-string
      "usage: leo [options] [file [args]]"
      ""
      "Available options are:"
      "  -v, --version  show version information"
      "  -h, --help     show this help message")))

(define (start $arguments)
  (syntax-case $arguments ()
    (() (display-help))
    (_ (start-non-empty $arguments))))

(define (start-non-empty $arguments)
  (syntax-case $arguments ()
    (("-v" . x)
      (display-version)
      (start-non-empty (datum x)))
    (("--version" . x)
      (display-version)
      (start-non-empty (datum x)))
    (("-h" . x)
      (display-help)
      (start-non-empty (datum x)))
    (("--help" . x)
      (display-help)
      (start-non-empty (datum x)))
    ((filename arg ...)
      (parameterize ((command-line-arguments (datum (arg ...))))
        (load-leo (datum filename))))
    (() (void))))

(start (command-line-arguments))
