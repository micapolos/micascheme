(library (sjasm runtime)
  (export extern line labeled)
  (import (micascheme))

  (define extern #f)
  (define (expr . $args) (newline))
  (define (line . $args) (newline))
  (define (labeled $label . $args) (newline))
)
