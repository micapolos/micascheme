(library (leo parser)
  (export
    parser parser? parser parser-push-fn parser-finish-fn
    parser-push parser-finish
    parser-of parser-bind parser-map
    parse

    char-parser
    string-parser
    exact-parser
    line-parser
    positive-integer-parser
    word-parser
    oneof-parser
    fold-parser
    stack-parser
    indent-parser
  )

  (import (micascheme))

  ; ----------------------------------------------------------

  (data (parser push-fn finish-fn))

  (define (parser-push $parser $char)
    ((parser-push-fn $parser) $char))

  (define (parser-finish $parser)
    ((parser-finish-fn $parser)))

  (define (parser-of $value)
    (parser 
      (lambda ($char) #f) 
      (lambda () $value)))

  (define (parser-bind $parser $fn)
    (parser
      (lambda ($char)
        (lets
          ($push-parser (parser-push $parser $char))
          (if $push-parser
            (parser-bind $push-parser $fn)
            (opt-lift parser-push
              (opt-lift $fn (parser-finish $parser))
              $char))))
      (lambda ()
        (opt-lift parser-finish
          (opt-lift $fn 
            (parser-finish $parser))))))

  (define (parser-map $parser $fn)
    (parser-bind $parser 
      (lambda ($item) 
        (parser-of ($fn $item)))))

  ; ----------------------------------------------------------

  (define (parse $parser $string)
    (opt-lift parser-finish
      (fold-left
        (lambda ($parser-opt $char)
          (opt-lift parser-push $parser-opt (opt $char)))
        $parser
        (string->list $string))))

  ; ----------------------------------------------------------

  (define (make-char-parser $char-opt)
    (parser
      (lambda ($char) 
        (and 
          (not $char-opt) 
          (make-char-parser $char)))
      (lambda () 
        $char-opt)))

  (define (char-parser)
    (make-char-parser #f))

  ; ----------------------------------------------------------

  (define (make-string-parser $char-stack)
    (parser
      (lambda ($char)
        (make-string-parser (push $char-stack $char)))
      (lambda ()
        (list->string (reverse $char-stack)))))

  (define (string-parser)
    (make-string-parser (stack)))

  ; ----------------------------------------------------------

  (define (make-exact-parser $string $index)
    (parser
      (lambda ($char)
        (and 
          (< $index (string-length $string))
          (char=? $char (string-ref $string $index))
          (make-exact-parser $string (+ $index 1))))
      (lambda () 
        (= $index (string-length $string)))))
          

  (define (exact-parser $string)
    (make-exact-parser $string 0))

  ; ----------------------------------------------------------

  (define (make-line-parser $char-stack-or-line)
    (parser
      (lambda ($char)
        (and (not (string? $char-stack-or-line))
          (make-line-parser
            (case $char
              ((#\newline) (list->string (reverse $char-stack-or-line)))
              (else (push $char-stack-or-line $char))))))
      (lambda ()
        (and (string? $char-stack-or-line) $char-stack-or-line))))

  (define (line-parser)
    (make-line-parser (stack)))

  ; ----------------------------------------------------------

  (define (make-positive-integer-parser $digit-stack)
    (parser
      (lambda ($char)
        (and
          (char-numeric? $char)
          (make-positive-integer-parser 
            (push 
              $digit-stack
              (- (char->integer $char) (char->integer #\0))))))
      (lambda () 
        (and
          (not (null? $digit-stack))
          (fold-left 
            (lambda ($integer $digit) (+ (* $integer 10) $digit))
            0
            (reverse $digit-stack))))))

  (define (positive-integer-parser)
    (make-positive-integer-parser (stack)))

  ; ----------------------------------------------------------

  (define (make-word-parser $letter-stack)
    (parser
      (lambda ($char)
        (and
          (char-alphabetic? $char)
          (make-word-parser (push $letter-stack $char))))
      (lambda () 
        (and
          (not (null? $letter-stack))
          (string->symbol (list->string (reverse $letter-stack)))))))

  (define (word-parser)
    (make-word-parser (stack)))

  ; ----------------------------------------------------------

  (define (make-oneof-parser $parsers)
    (and (not (null? $parsers))
      (parser
        (lambda ($char)
          (make-oneof-parser
            (filter-opts
              (map
                (lambda ($parser) (parser-push $parser $char))
                $parsers))))
        (lambda ()
          (single (filter-opts (map parser-finish $parsers)))))))

  (define-syntax-rule (oneof-parser $parser ...)
    (make-oneof-parser (list $parser ...)))

  ; ----------------------------------------------------------

  (define (fold-parser $folded $parser $fn)
    (oneof-parser
      (parser-of $folded)
      (parser-bind $parser
        (lambda ($item)
          (fold-parser ($fn $folded $item) $parser $fn)))))

  (define (stack-parser $parser)
    (fold-parser (stack) $parser push))

  ; ----------------------------------------------------------

  (define indent-size 2)

  (define (make-indent-parser $indent $parser)
    (parser
      (lambda ($char)
        (case $char
          ((#\space)
            (if (< $indent indent-size)
              (make-indent-parser (+ $indent 1) $parser)
              (opt-lift make-indent-parser 
                (opt $indent)
                (parser-push $parser $char))))
          ((#\newline)
            (and 
              (or (= $indent 0) (= $indent indent-size)) 0)
              (opt-lift make-indent-parser 0 (parser-push $parser $char)))
          (else 
            (and
              (= $indent indent-size)
              (opt-lift make-indent-parser
                (opt $indent)
                (parser-push $parser $char))))))
      (lambda ()
        (and 
          (zero? $indent)
          (parser-finish $parser)))))

  (define (indent-parser $parser)
    (make-indent-parser 0 $parser))
)