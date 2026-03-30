(library (leo start)
  (export start)
  (import
    (micascheme)
    (leo leo)
    (leo load)
    (leo version)
    (leo exception-handler)
    (prefix (leo scheme) %))

  (define (start $arguments)
    (syntax-case $arguments ()
      (() (start-repl))
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
      (("--assembly-output" . x)
        (start-assembly-output (datum x)))
      (("--optimize-level" . x)
        (start-optimize-level (datum x)))
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
          "Available options:"
          "  -v  --version            show version information"
          "  -h  --help               show this help message"
          "      --assembly-output    show assembly output"
          "      --optimize-level n   set optimize level < 0 | 1 | 2 | 3 >"))
      (start-options $arguments)))

  (define (start-assembly-output $arguments)
    (parameterize ((($primitive $assembly-output) #t))
      (start-options $arguments)))

  (define (start-optimize-level $arguments)
    (parameterize ((optimize-level (string->number (car $arguments))))
      (start-options (cdr $arguments))))

  (define (start-file $file $arguments)
    (parameterize
      ((command-line-arguments $arguments))
      (leo
        (with-exception-handler
          leo-exception-handler
          (lambda ()
            (load-leo-program $file))))))

  (define (start-repl)
    (leo
      (eval
        (cons sc-expand
          '(let ()
            (import (leo repl))
            (leo-repl))))))
)
