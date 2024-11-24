(library (micalog syntax)
  (export micalog-syntax)
  (import
    (prefix (micascheme) %)
    (micalog type)
    (micalog keywords))
  (%export (import (micalog keywords)))

  (%define-syntax (micalog-syntax $syntax $lookup)
    (%syntax-case $syntax ()
      ((_ mod)
        (%fluent $lookup
          (scope-module->typed-syntax (%syntax mod))
          (%unsyntax)
          (%syntax)
          (%quasisyntax)))))
)
