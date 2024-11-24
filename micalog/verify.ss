(library (micalog verify)
  (export micalog-verify)
  (import
    (prefix (micascheme) %)
    (prefix (micalog type) %)
    (micalog keywords))
  (%export (import (micalog keywords)))

  (%define-case-syntax (micalog-verify module)
    (%fluent
      (%syntax module)
      (%module->typed-syntax)
      (%ignore (%syntax (%void)))))
)
