(library (micalog run)
  (export micalog-run)
  (import
    (prefix (micascheme) %)
    (micalog scheme transformer)
    (micalog core type)
    (micalog emu on-old-new)
    (micalog emu inits-updates))
  (%export (import (micalog keywords)))
  (%export (import (micalog scheme keywords)))

  (%define-syntax (micalog-run $syntax $lookup)
    (%syntax-case $syntax ()
      ((_ module)
        (%quasisyntax
          (%unsyntax
            (%fluent $lookup
              (lookup+core)
              (lookup-module->typed-syntax (%syntax module))
              (module->on-old-new-syntax)
              (module->inits-updates-syntax)
              (module->scheme)
              (%let $it
                (%let ()
                  (%pretty-print (%syntax->datum $it))
                  $it))))))))
)
