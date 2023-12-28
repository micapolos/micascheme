(library (stax)
  (export
    poke peek
    resize load store
    push pop)
  (import
    (except (micascheme) push pop load))

  (define mem (make-bytevector #x10000 0))
  (define sp #xFFFF)

  (define (poke $addr $byte)
    (bytevector-u8-set! mem $addr $byte))

  (define (peek $addr)
    (bytevector-u8-ref mem $addr))

  (define (load $offset)
    (peek (+ sp $offset)))

  (define (store $offset $byte)
    (poke (+ sp $offset) $byte))

  (define (resize $offset)
    (set! sp (- sp $offset)))

  (define (push $byte)
    (run
      (store 0 $byte)
      (resize 1)))

  (define (pop)
    (run
      (resize -1)
      (load 0)))
)
