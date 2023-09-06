(import
  (except (micascheme) compile)
  (leo value)
  (leo compiler))

; -----------------------------------------

(check
  (obj=?
    (compile (compiler!) (typed `v `t))
    (typed `v `t)))

; -----------------------------------------

(check
  (obj=?
    (compile (compiler!) #t)
    (typed #t boolean!)))

(check
  (obj=?
    (compile (compiler!) 128)
    (typed 128 number!)))

(check
  (obj=?
    (compile (compiler!) "foo")
    (typed "foo" string!)))

; -----------------------------------------

(check
  (obj=?
    (compile (compiler!) (typeof (typed `v `t)))
    (typed `t type!)))

; -----------------------------------------

(check
  (obj=?
    (compile (compiler!) (named! foo (typed `v1 `t1)))
    (typed `v1 (named! foo `t1))))

; -----------------------------------------

(check
  (obj=?
    (compile (compiler!) (named-get (typed `v (named! foo `t))))
    (typed `v `t)))

; -----------------------------------------

(check
  (obj=?
    (compile (compiler!) (tuple!))
    (typed `(void) (tuple!))))

(check
  (obj=?
    (compile (compiler!) (tuple! (typed `v1 `t1)))
    (typed `v1 (tuple! `t1))))

(check
  (obj=?
    (compile (compiler!) (tuple! (typed `v1 `t1) (typed `v2 `t2)))
    (typed `(cons v1 v2) (tuple! `t1 `t2))))

(check
  (obj=?
    (compile (compiler!) (tuple! (typed `v1 `t1) (typed `v2 `t2) (typed `v3 `t3)))
    (typed `(vector v1 v2 v3) (tuple! `t1 `t2 `t3))))

; -----------------------------------------

(check
  (obj=?
    (compile (compiler!) (tuple-get (typed `v (tuple! `t1)) `t1))
    (typed `v `t1)))

(check
  (obj=?
    (compile (compiler!) (tuple-get (typed `v (tuple! `t1 `t2)) `t1))
    (typed `(car v) `t1)))

(check
  (obj=?
    (compile (compiler!) (tuple-get (typed `v (tuple! `t1 `t2)) `t2))
    (typed `(cdr v) `t2)))

(check
  (obj=?
    (compile (compiler!) (tuple-get (typed `v (tuple! `t1 `t2 `t3)) `t1))
    (typed `(vector-ref v 0) `t1)))

(check
  (obj=?
    (compile (compiler!) (tuple-get (typed `v (tuple! `t1 `t2 `t3)) `t2))
    (typed `(vector-ref v 1) `t2)))

(check
  (obj=?
    (compile (compiler!) (tuple-get (typed `v (tuple! `t1 `t2 `t3)) `t3))
    (typed `(vector-ref v 2) `t3)))

; -----------------------------------------

(check
  (obj=?
    (compile (compiler!)
      (choice-switch! (typed `choice (choice! `t1))
        (typeof (variable `t1))))
    (typed
      `(lets (v0 choice) t1)
      type!)))

(check
  (obj=?
    (compile (compiler!)
      (choice-switch! (typed `choice (choice! `t1 `t2))
        (typeof (variable `t1))
        (typeof (variable `t2))))
    (typed
      `(lets
        ($tmp choice)
        (v0 (cdr $tmp))
        (if (car $tmp) t1 t2))
      type!)))

(check
  (obj=?
    (compile (compiler!)
      (choice-switch! (typed `choice (choice! `t1 `t2 `t3))
        (typeof (variable `t1))
        (typeof (variable `t2))
        (typeof (variable `t3))))
    (typed
      `(lets
        ($tmp choice)
        (v0 (cdr $tmp))
        (case (car $tmp)
          ((0) t1)
          ((1) t2)
          (else t3)))
      type!)))

; -----------------------------------------

(check
  (obj=?
    (compile (compiler! `t1 `t2) (variable `t1))
    (typed `v0 `t1)))

(check
  (obj=?
    (compile (compiler! `t1! `t2) (variable `t2))
    (typed `v1 `t2)))

; -----------------------------------------

(check
  (obj=?
    (compile (compiler!) (function! (`t1 `t2) (variable `t1)))
    (typed
      `(lambda (v0 v1) v0)
      (function! (`t1 `t2) `t1))))

(check
  (obj=?
    (compile (compiler!) (function! (`t1 `t2) (variable `t2)))
    (typed
      `(lambda (v0 v1) v1)
      (function! (`t1 `t2) `t2))))

; -----------------------------------------

(check
  (obj=?
    (compile
      (compiler!)
      (application!
        (typed `fn (function! (`t1 `t2) `t3))
        (typed `v1 `t1) (typed `v2 `t2)))
    (typed
      `(fn v1 v2)
      `t3)))
