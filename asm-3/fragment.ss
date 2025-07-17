(library (asm-3 fragment)
  (export
    fragment fragment? fragment-dep-stack fragment-ref
    fragment-with
    fragment->datum
    list->fragment
    fragment-append
    fragment-map
    check-fragment)
  (import (micascheme))

  (data (fragment dep-stack ref))

  (define-rule-syntax (fragment-with (dep ...) ref)
    (fragment (stack #'dep ...) ref))

  (define (fragment+dep $fragment $dep)
    (lets
      ($dep-stack (fragment-dep-stack $fragment))
      (fragment-with-dep-stack $fragment
        (cond
          ((memp (partial free-identifier=? $dep) $dep-stack) $dep-stack)
          (else (push $dep-stack $dep))))))

  (define (fragment->datum $fragment)
    `(fragment-with
      (,@(reverse (map syntax->datum (fragment-dep-stack $fragment))))
      ,(fragment-ref $fragment)))

  (define (list->fragment $fragments)
    (fragment
      (reverse (dedup free-identifier=? (apply append (map (dot reverse fragment-dep-stack) $fragments))))
      (map fragment-ref $fragments)))

  (define (fragment-append . $fragments)
    (list->fragment $fragments))

  (define (fragment-map $proc $fragment)
    (fragment-with-ref $fragment
      ($proc (fragment-ref $fragment))))

  (define-rule-syntax (check-fragment in out)
    (check (equal? (fragment->datum in) (fragment->datum out))))
)
