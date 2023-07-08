(library (parser)
  (export
    parser 
    parser-value 
    parser-fn 
    parser?
    char
    the
    token
    parse)

  (import (micascheme))

  (data parser value fn)
  (data partial value size)
  (data parse-error value pos)

  (define (the value)
    (parser value
      (lambda (char) (void))))

  (define char 
    (parser (partial 'char 0) 
      (lambda (ch) (the ch))))

  (define token
    (case-lambda
      ((s) (token s 0))
      ((s pos) 
        (parser 
          (partial `(token ,s) pos)
          (lambda (char)
            (if (eq? char (string-ref s pos))
              (token s (+ pos 1))
              (void)))))))

  (define (parse-pos parser string pos)
    (if (= (string-length string) pos)
      (let ((value (parser-value parser)))
        (cond
          ((partial? value) 
            (error `parse 
              (format
                "Expected ~s at ~s" 
                (partial-value value)
                (- pos (partial-size value)))))
          (else value)))
      (lets 
        (value (parser-value parser))
        (consume (parser-fn parser))
        (consumed (consume (string-ref string pos)))
        (cond
          ((parser? consumed)
            (parse-pos consumed string (+ pos 1)))
          (else
            (error `parse
              (format
                "Expected ~s at ~s" 
                consumed pos)))))))

  (define (parse parser string)
    (parse-pos parser string 0))
)