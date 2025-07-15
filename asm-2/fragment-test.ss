(import (micascheme) (asm-2 fragment) (syntax lookup))

(check
  (equal?
    (fragment-ref
      (fragment-with 10)
      (empty-lookup))
    10))

(check
  (equal?
    (fragment-ref
      (fragment-with (dep a b) (string-append (dep a) (dep b)))
      (lookup-with (a "a") (b "b")))
    "ab"))

(check
  (equal?
    (fragment-ref
      (fragment-map
        (lambda ($string) (string-append $string "!"))
        (fragment-with (dep a b) (string-append (dep a) (dep b))))
      (lookup-with (a "a") (b "b")))
    "ab!"))

(check
  (equal?
    (fragment-ref
      (fragment-with (dep a b) (cons (dep a) (dep b)))
      (lookup-with (a "a") (b "b")))
    '("a" . "b")))

(check
  (equal?
    (fragment-ref
      (fragment-append
        (fragment-with (dep a b)
          (cons (dep a) (dep b)))
        (fragment-with (dep b c)
          (cons (dep b) (dep c)))
        (fragment-with (dep c d)
          (cons (dep c) (dep d))))
      (lookup-with
        (a "a")
        (b "b")
        (c "c")
        (d "d")))
    '(("a" . "b") ("b" . "c") ("c" . "d"))))

(check
  (for-all* free-identifier=?
    (fragment-deps
      (fragment-append
        (fragment-with (dep a b)
          (cons (dep a) (dep b)))
        (fragment-with (dep b c)
          (cons (dep b) (dep c)))
        (fragment-with (dep c d)
          (cons (dep c) (dep d)))))
    (syntaxes a b b c c d)))
