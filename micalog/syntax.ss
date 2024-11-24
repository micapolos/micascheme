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
        (%quasisyntax
          (%syntax
            (%unsyntax
              (scope-module->typed-syntax $lookup (%syntax mod))))))))
)
