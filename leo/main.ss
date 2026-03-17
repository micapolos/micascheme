(import
  (leo scheme)
  (leo load)
  (leo version)
  (only (micascheme) lines-string))

(define (start $arguments)
  (syntax-case $arguments ()
    (() (start-help $arguments))
    (_ (start-non-empty $arguments))))

(define (start-non-empty $arguments)
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
    (() (void))))

(define (start-version $arguments)
  (displayln (string-append "Leo " version))
  (start-non-empty $arguments))

(define (start-help $arguments)
  (display
    (lines-string
      "usage: leo [options] [file [args]]"
      ""
      "Available options are:"
      "  -v, --version  show version information"
      "  -h, --help     show this help message"))
  (start-non-empty $arguments))

(define (start-file $file $arguments)
  (parameterize ((command-line-arguments $arguments))
    (load-leo $file)))

(start (command-line-arguments))
