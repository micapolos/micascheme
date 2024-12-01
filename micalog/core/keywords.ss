(library (micalog core keywords)
  (export variable literal)
  (import
    (only (micascheme) define-aux-keywords export)
    (micalog keywords))
  (export (import (micalog keywords)))

  (define-aux-keywords variable literal)
)
