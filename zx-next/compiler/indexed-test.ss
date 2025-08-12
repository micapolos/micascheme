(import (zx-next test) (zx-next compiler indexed))

(define-fragments
  (test-data (dw #x1234))
  (foo-string (dz "foo"))
  (bar-string (dz "bar")))

(test
  (case const-1
    (indexed (assert 1 (native (ld a #x12)) #x12)))

  (case const-1
    (indexed (assert 1 (const #x22) #x22)))

  (case const-2
    (indexed (assert 2 (const #x1234) #x1234)))

  (case const-3
    (indexed (assert 3 (const #x123456) #x123456)))

  (case const-4
    (indexed (assert 4 (const #x12345678) #x12345678)))

  (case inc-1
    (indexed (assert 1 (inc (const #x11)) #x12)))

  (case dec-1
    (indexed (assert 1 (dec (const #x11)) #x10)))

  (case inc-2
    (indexed (assert 2 (inc (const #x1122)) #x1123)))

  (case dec-2
    (indexed (assert 2 (dec (const #x1122)) #x1121)))

  (case neg-1
    (indexed (assert 1 (neg (const #x22)) #xde)))

  (case cpl-1
    (indexed (assert 1 (cpl (const #x22)) #xdd)))

  (case add-const-1
    (indexed (assert 1 (add-const (const #x11) #x22) #x33)))

  (case add-1
    (indexed (assert 1 (add (const #x11) (const #x22)) #x33)))

  (case sub-const-1
    (indexed (assert 1 (sub-const (const #x33) #x22) #x11)))

  (case sub-1
    (indexed (assert 1 (sub (const #x33) (const #x22)) #x11)))

  (case and-const-1
    (indexed (assert 1 (and-const (const #x33) #x0f) #x03)))

  (case and-1
    (indexed (assert 1 (and (const #x33) (const #x0f)) #x03)))

  (case or-const-1
    (indexed (assert 1 (or-const (const #x33) #x0f) #x3f)))

  (case or-1
    (indexed (assert 1 (or (const #x33) (const #x0f)) #x3f)))

  (case xor-const-1
    (indexed (assert 1 (xor-const (const #x33) #x0f) #x3c)))

  (case xor-1
    (indexed (assert 1 (xor (const #x33) (const #x0f)) #x3c)))

  (case mul-1
    (indexed (assert 2 (mul (const #x02) (const #x03)) #x0006)))

  (case peek-const
    (indexed (assert 1 (peek-const test-data) #x34)))

  (case peek-const+1
    (indexed (assert 1 (peek-const (+ test-data 1)) #x12)))

  (case peek
    (indexed (assert 1 (peek 2 (const test-data)) #x34)))

  (case peek+1
    (indexed (assert 1 (peek 2 (inc (const test-data))) #x12)))

  (case peek-offset
    (preserve (ix)
      (ld ix test-data)
      (indexed (assert 1 (peek-offset 0) #x34))))

  (case peek-offset+1
    (preserve (ix)
      (ld ix test-data)
      (indexed (assert 1 (peek-offset 1) #x12))))

  (case if-zero?-1-positive
    (indexed (assert 1 (if 1 (zero? (const #x00)) (const #x34) (const #x56)) #x34)))

  (case if-zero?-1-negative
    (indexed (assert 1 (if 1 (zero? (const #x12)) (const #x34) (const #x56)) #x56)))

  (case if-eq?-1-positive
    (indexed (assert 1 (if 1 (eq? (const #x01) (const #x01)) (const #x34) (const #x56)) #x34)))

  (case if-eq?-1-negative
    (indexed (assert 1 (if 1 (eq? (const #x01) (const #x02)) (const #x34) (const #x56)) #x56)))

  (case if-gt?-1-positive
    (indexed (assert 1 (if 1 (gt? (const #x03) (const #x02)) (const #x34) (const #x56)) #x34)))

  (case if-gt?-1-negative
    (indexed (assert 1 (if 1 (gt? (const #x02) (const #x02)) (const #x34) (const #x56)) #x56)))

  (case local-2-1
    (ld-indexed hl () () 2
      (call-frame
        (native
          (push #x3412)
          (push #x7856))
        (local 0)))
    (assert hl #x1234))

  (case local-1-1
    (ld-indexed a () (2) 1
      (call-frame
        (native
          (push #x3412)
          (push #x7856))
        (local 1)))
    (assert a #x56))

  (case local-1-2
    (push #x3412)
    (push #x7856)
    (ld-indexed a () (2 1) 1
      (call-frame
        (native
          (push #x3412)
          (push #x7856))
        (local 2)))
    (assert a #x78))

  (case arg-2-0
    (push #x3412)
    (push #x7856)
    (preserve (af) ; fake return address
      (ld-indexed hl () () 2 (call-frame (arg 0)))
      (assert hl #x5678)))

  (case arg-1-1
    (push #x3412)
    (push #x7856)
    (preserve (af) ; fake return address
      (ld-indexed a (2) () 1 (call-frame (arg 1)))
      (assert a #x34)))

  (case arg-1-2
    (push #x3412)
    (push #x7856)
    (preserve (af) ; fake return address
      (ld-indexed a (2 1) () 1 (call-frame (arg 2)))
      (assert a #x12)))

  (case call-0
    (indexed (write-newline)))

  (case call-1
    (indexed (write-char (1 (const #\a)))))

  (case call-2
    (indexed (write-string (2 (const foo-string)))))

  (case calls
    (indexed
      (write-string (2 (const foo-string)))
      (write-char (1 (const #\space)))
      (write-string (2 (const bar-string)))
      (write-char (1 (const #\return)))))

  (case push-1
    (ld-indexed void 1 (push (const #x12)))
    (pop a)
    (assert a #x12))

  (case push-2
    (ld-indexed void 2 (push (const #x1234)))
    (pop hl)
    (assert hl #x1234))

  (case push-3
    (ld-indexed void 3 (push (const #x123456)))
    (pop lde)
    (assert l #x12)
    (assert de #x3456))

  (case push-4
    (ld-indexed void 4 (push (const #x12345678)))
    (pop hlde)
    (assert hl #x1234)
    (assert de #x5678))

  (case pop-1
    (ld a #x12)
    (push a)
    (indexed (assert 1 (pop) #x12)))

  (case pop-2
    (ld hl #x1234)
    (push hl)
    (indexed (assert 2 (pop) #x1234)))

  (case pop-3
    (ld a #x12)
    (push a)
    (ld hl #x3456)
    (push hl)
    (indexed (assert 3 (pop) #x123456)))

  (case pop-4
    (ld hl #x1234)
    (push hl)
    (ld hl #x5678)
    (push hl)
    (indexed (assert 4 (pop) #x12345678)))

  (case lets/empty
    (indexed (assert 2 (lets (const #x1234))  #x1234)))

  (case lets-1-1
    (indexed
      (assert 1
        (call-frame
          (lets
            (1 (const #x12))
            (local 0)))
        #x12)))

  (case lets-2-2
    (indexed
      (assert 1
        (call-frame
          (lets
            (1 (const #x46))
            (1 (const #x34))
            (sub (local 0) (local 1))))
        #x12)))

  ;(call wait-space)
)
