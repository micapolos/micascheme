(library (leo get)
  (export
    symbol-getter)
  (import
    (micascheme)
    (get))

  (define (push-symbol-chars-getter $chars)
    (getter-lets
      ($char? char?-getter)
      (switch $char?
        ((false? _)
          (getter $chars))
        ((char-alphabetic? $char-alphabetic)
          (push-symbol-chars-getter
            (push $chars $char-alphabetic)))
        ((else $char-other)
          (getter-lets
            (_ (char-ungetter $char-other))
            (getter $chars))))))

  (define symbol-getter
    (getter-lets
      ($chars (push-symbol-chars-getter (stack)))
      (switch $chars
        ((null? _)
          (throw null-symbol-getter))
        ((else $chars)
          (getter (string->symbol (apply string (reverse $chars))))))))

)
