(library (micalog core keywords)
  (export variable literal)
  (import
    (only (micascheme) define-keywords export)
    (micalog keywords))
  (export (import (micalog keywords)))

  (define-keywords variable literal)
)
