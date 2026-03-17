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
      (start-non-empty (cdr $arguments)))
    (("--version" . x)
      (display-version)
      (start-non-empty (cdr $arguments)))
    (("-h" . x)
      (display-help)
      (start-non-empty (cdr $arguments)))
    (("--help" . x)
      (display-help)
      (start-non-empty (cdr $arguments)))
    ((filename . x)
      (parameterize ((command-line-arguments (cdr $arguments)))
        (load-leo #'filename)))
    (() (void))))

(start (command-line-arguments))
