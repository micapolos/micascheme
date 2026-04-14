(library
  (leo syntax)
  (export)
  (import (scheme))
  (export
    (import
      (only (scheme)
        syntax
        quasisyntax
        unsyntax
        unsyntax-splicing)
      (rename
        (only (syntax)
          syntax?
          syntax=?
          define-keyword
          define-keywords)
        (syntax=? free-syntax=?)))))
