(library (zexy z80)
  (export
    z80 z80? make-z80
    z80-run)
  (import
    (except (micascheme) define)
    (only (scheme) define)
    (zexy unit))

  (define-unit z80
    (fields
      (unsigned-8 a)
      (unsigned-8 f)
      (unsigned-8 b)
      (unsigned-8 c)
      (unsigned-8 d)
      (unsigned-8 e)
      (unsigned-8 h)
      (unsigned-8 l)

      (unsigned-16 pc)
      (unsigned-16 sp)))

  (define (z80-run z80 mem mem! io io!)
    (define-rule-syntax (define-macro $params $body ...)
      (define-rule-syntax $params
        (begin $body ...)))

    (define-macro (a) (z80-a z80))
    (define-macro (f) (z80-f z80))
    (define-macro (b) (z80-b z80))
    (define-macro (c) (z80-c z80))
    (define-macro (d) (z80-d z80))
    (define-macro (e) (z80-e z80))
    (define-macro (h) (z80-h z80))
    (define-macro (l) (z80-l z80))

    (define-macro (pc) (z80-pc z80))
    (define-macro (sp) (z80-sp z80))

    (define-macro (a! v) (set-z80-a! z80 v))
    (define-macro (f! v) (set-z80-f! z80 v))
    (define-macro (b! v) (set-z80-b! z80 v))
    (define-macro (c! v) (set-z80-c! z80 v))
    (define-macro (d! v) (set-z80-d! z80 v))
    (define-macro (e! v) (set-z80-e! z80 v))
    (define-macro (h! v) (set-z80-h! z80 v))
    (define-macro (l! v) (set-z80-l! z80 v))

    (define-macro (pc! v) (set-z80-pc! z80 v))
    (define-macro (sp! v) (set-z80-sp! z80 v))

    (define-macro (fx-nm fx) (fxand fx #xffff))
    (define-macro (fx-n fx) (fxand fx #xff))

    (define-macro (nm lsb msb) (fxior (fxsll msb 8) lsb))
    (define-macro (lsb nm) (fxand nm #xff))
    (define-macro (msb nm) (fxsrl nm 8))

    (define-macro (rr rh rl) (nm (rl) (rh)))

    (define-macro (bc) (rr b c))
    (define-macro (de) (rr d e))
    (define-macro (hl) (rr h l))

    (define-macro (rr! rh! rl! expr)
      (let ((v expr))
        (rh! (msb v))
        (rl! (lsb v))))

    (define-macro (bc! v) (rr! b! c! v))
    (define-macro (de! v) (rr! d! e! v))
    (define-macro (hl! v) (rr! h! l! v))

    (define-macro (inc-nm nm) (fx-nm (fx+ nm 1)))
    (define-macro (dec-nm nm) (fx-nm (fx- nm 1)))

    (define halt? #f)

    (define-macro (fetch)
      (let ((n (mem (pc))))
        (pc! (inc-nm (pc)))
        n))

    (define-macro (fetch-nm)
      (lets
        (lsb (fetch))
        (msb (fetch))
        (nm lsb msb)))

    (do ()
      (halt? (void))

      (case1 (fetch)
        (#x00 (void))
        (#x01 (bc! (fetch-nm)))
        (#x06 (b! (fetch)))
        (#x0e (c! (fetch)))

        (#x11 (de! (fetch-nm)))
        (#x16 (d! (fetch)))
        (#x1e (e! (fetch)))

        (#x21 (hl! (fetch-nm)))
        (#x26 (h! (fetch)))
        (#x2e (l! (fetch)))

        (#x31 (sp! (fetch-nm)))
        (#x36 (mem! (hl) (fetch)))
        (#x3e (a! (fetch)))

        (#x40 (b! (b)))
        (#x41 (b! (c)))
        (#x42 (b! (d)))
        (#x43 (b! (e)))
        (#x44 (b! (h)))
        (#x45 (b! (l)))
        (#x46 (b! (mem (hl))))
        (#x47 (b! (a)))

        (#x48 (c! (b)))
        (#x49 (c! (c)))
        (#x4a (c! (d)))
        (#x4b (c! (e)))
        (#x4c (c! (h)))
        (#x4d (c! (l)))
        (#x4e (c! (mem (hl))))
        (#x4f (c! (a)))

        (#x50 (d! (b)))
        (#x51 (d! (c)))
        (#x52 (d! (d)))
        (#x53 (d! (e)))
        (#x54 (d! (h)))
        (#x55 (d! (l)))
        (#x56 (d! (mem (hl))))
        (#x57 (d! (a)))

        (#x58 (e! (b)))
        (#x59 (e! (c)))
        (#x5a (e! (d)))
        (#x5b (e! (e)))
        (#x5c (e! (h)))
        (#x5d (e! (l)))
        (#x5e (e! (mem (hl))))
        (#x5f (e! (a)))

        (#x60 (h! (b)))
        (#x61 (h! (c)))
        (#x62 (h! (d)))
        (#x63 (h! (e)))
        (#x64 (h! (h)))
        (#x65 (h! (l)))
        (#x66 (h! (mem (hl))))
        (#x67 (h! (a)))

        (#x68 (l! (b)))
        (#x69 (l! (c)))
        (#x6a (l! (d)))
        (#x6b (l! (e)))
        (#x6c (l! (h)))
        (#x6d (l! (l)))
        (#x6e (l! (mem (hl))))
        (#x6f (l! (a)))

        (#x70 (mem! (hl) (b)))
        (#x71 (mem! (hl) (c)))
        (#x72 (mem! (hl) (d)))
        (#x73 (mem! (hl) (e)))
        (#x74 (mem! (hl) (h)))
        (#x75 (mem! (hl) (l)))
        (#x76 (set! halt? #t))
        (#x77 (mem! (hl) (a)))

        (#x78 (a! (b)))
        (#x79 (a! (c)))
        (#x7a (a! (d)))
        (#x7b (a! (e)))
        (#x7c (a! (h)))
        (#x7d (a! (l)))
        (#x7e (a! (mem (hl))))
        (#x7f (a! (a)))

        (#xed
          (case1 (fetch)
            (#x41 (io! (bc) (b)))
            (#x49 (io! (bc) (c)))
            (#x51 (io! (bc) (d)))
            (#x59 (io! (bc) (e)))
            (#x61 (io! (bc) (h)))
            (#x69 (io! (bc) (l)))
            (#x71 (io! (bc) 0))
            (#x79 (io! (bc) (a)))
            ((else op) (throw illegal-ed-op op))))

        ((else op) (throw illegal-op op))
      )))
)
