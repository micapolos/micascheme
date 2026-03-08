(library (leo get)
  (export word?-getter)
  (import
    (micascheme)
    (get))

  (define (push-word-chars-getter $chars)
    (getter-lets
      ($char? char?-getter)
      (switch $char?
        ((false? _)
          (getter $chars))
        ((char-alphabetic? $char-alphabetic)
          (push-word-chars-getter
            (push $chars $char-alphabetic)))
        ((else $char-other)
          (getter-lets
            (_ (char-ungetter $char-other))
            (getter $chars))))))

  (define word?-getter
    (getter-lets
      ($chars (push-word-chars-getter (stack)))
      (switch $chars
        ((null? _)
          (getter #f))
        ((else $chars)
          (getter (string->symbol (apply string (reverse $chars))))))))

)
