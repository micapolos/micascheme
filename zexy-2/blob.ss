(library (zexy-2 blob)
  (export
    u16-blob
    u2-u3-u3-blob)
  (import
    (except (micascheme) put-u16)
    (zexy-2 put)
    (zexy-2 value))

  (define (u16-blob $word)
    (blob 2
      (lambda ($port)
        (put-u16 $port $word))))

  (define (u2-u3-u3-blob $a $b $c)
    (u8-blob (u2-u3-u3->u8 $a $b $c)))
)
