(import
  (micascheme)
  (leo value)
  (leo decompiler))

(check
  (equal?
    (decompile (empty-decompiler)
      (typed `v1 `t1))
    (typed `v1 `t1)))

(check
  (equal?
    (decompile (empty-decompiler)
      (typed #t boolean!))
    #t))

(check
  (equal?
    (decompile (empty-decompiler)
      (typed 128 number!))
    128))

(check
  (equal?
    (decompile (empty-decompiler)
      (typed "foo" string!))
    "foo"))

(check
  (equal?
    (decompile (empty-decompiler)
      (typed "foo" (named! foo string!)))
    (named! foo "foo")))

(check
  (equal?
    (decompile (empty-decompiler)
      (typed (void) (tuple!)))
    (tuple!)))

(check
  (equal?
    (decompile (empty-decompiler)
      (typed 128 (tuple! number!)))
    (tuple! 128)))

(check
  (equal?
    (decompile (empty-decompiler)
      (typed (cons 128 "foo") (tuple! number! string!)))
    (tuple! 128 "foo")))

(check
  (equal?
    (decompile (empty-decompiler)
      (typed (vector 128 "foo" #t) (tuple! number! string! boolean!)))
    (tuple! 128 "foo" #t)))

(check
  (equal?
    (decompile (empty-decompiler)
      (typed 128 (choice! number!)))
    128))

(check
  (equal?
    (decompile (empty-decompiler)
      (typed (cons #t 128) (choice! number! string!)))
    128))

(check
  (equal?
    (decompile (empty-decompiler)
      (typed (cons #f "foo") (choice! number! string!)))
    "foo"))

(check
  (equal?
    (decompile (empty-decompiler)
      (typed (cons 0 128) (choice! number! string! boolean!)))
    128))

(check
  (equal?
    (decompile (empty-decompiler)
      (typed (cons 1 "foo") (choice! number! string! boolean!)))
    "foo"))

(check
  (equal?
    (decompile (empty-decompiler)
      (typed (cons 2 #t) (choice! number! string! boolean!)))
    #t))
