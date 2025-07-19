(library (asm-3 base)
  (export define-scoped)
  (import (micascheme) (keyword))
  (export
    (import
      (except (micascheme)
        environment
        environment?)))

  (define-rule-syntax (defines id ...) (begin))
  (define-rule-syntax (expects id ...) (begin))

  (expects
    empty-scope
    scope-append)
  (defines
    scoped scoped? scoped-scope scoped-ref
    scoped-with-scope scoped-with-ref
    pure-scoped
    scoped-map
    scoped-append
    list->scoped)
  (define-case-syntax (define-scoped (scoped scope))
    (lets
      ($scoped-map (identifier-append #'scoped #'scoped #'- #'map))
      ($scoped-scope (identifier-append #'scoped #'scoped #'- #'scope))
      ($scoped-ref (identifier-append #'scoped #'scoped #'- #'ref))
      ($pure-scoped (identifier-append #'scoped #'pure #'- #'scoped))
      ($empty-scope (identifier-append #'scoped #'empty #'- #'scope))
      ($scope-append (identifier-append #'scoped #'scope #'- #'append))
      #`(begin
        (data (scoped scope ref))
        (define (#,$scoped-map $proc $scoped)
          (scoped
            (#,$scoped-scope $scoped)
            ($proc (#,$scoped-ref $scoped))))
        (define (#,$pure-scoped $ref)
          (scoped (#,$empty-scope) $ref))
        (define-list->/append (scoped $list)
          (scoped
            (apply #,$scope-append (map #,$scoped-scope $list))
            (map #,$scoped-ref $list))))))
)
