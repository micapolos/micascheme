(library (stax)
  (export
    poke peek
    push pop)
  (import
    (except (micascheme) push pop))

  (define mem (make-bytevector #x10000 0))
  (define sp #xFFFF)

  (define (poke $addr $byte)
    (bytevector-u8-set! mem $addr $byte))

  (define (peek $addr)
    (bytevector-u8-ref mem $addr))

  (define (push $byte)
    (run
      (poke sp $byte)
      (set! sp (sub1 sp))))

  (define (pop)
    (run
      (set! sp (add1 sp))
      (peek sp)))
)
