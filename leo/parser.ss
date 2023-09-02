(library (leo parser)
  (export
    parser parser? parser-state-opt parser-push-fn parser-finish-fn
    parser-process

    parser-of
    string-parser
    line-parser
    positive-integer-parser
    word-parser
    oneof-parser
    indent-parser
  )

  (import (micascheme))

  ; ----------------------------------------------------------

  (data (parser state-opt push-fn finish-fn))

  (define (parser-process $parser $string)
    (lets
      ($state-opt (parser-state-opt $parser))
      ($push-fn (parser-push-fn $parser))
      ($finish-fn (parser-finish-fn $parser))
      ($state-opt
        (fold-left
          (lambda ($state-opt $char)
            (and $state-opt ($push-fn $state-opt $char)))
          $state-opt
          (string->list $string)))
      (and $state-opt ($finish-fn $state-opt))))

  ; ----------------------------------------------------------

  (define (parser-of $value)
    (parser
      `()
      (lambda (_ $char) #f)
      (lambda (_) $value)))

  ; ----------------------------------------------------------

  (define (string-parser)
    (parser
      (stack)
      (lambda ($char-stack $char) 
        (push $char-stack $char))
      (lambda ($char-stack) 
        (list->string (reverse $char-stack)))))

  ; ----------------------------------------------------------

  (define (line-parser)
    (parser
      (stack)
      (lambda ($char-stack-or-line $char)
        (and (not (string? $char-stack-or-line))
          (case $char
            ((#\newline) (list->string (reverse $char-stack-or-line)))
            (else (push $char-stack-or-line $char)))))
      (lambda ($char-stack-or-line)
        (and (string? $char-stack-or-line) $char-stack-or-line))))

  ; ----------------------------------------------------------

  (define (positive-integer-parser)
    (parser
      (stack)
      (lambda ($digit-stack $char)
        (and
          (char-numeric? $char)
          (push 
            $digit-stack
            (- (char->integer $char) (char->integer #\0)))))
      (lambda ($digit-stack) 
        (and
          (not (null? $digit-stack))
          (fold-left 
            (lambda ($integer $digit) (+ (* $integer 10) $digit))
            0
            (reverse $digit-stack))))))

  ; ----------------------------------------------------------

  (define (word-parser)
    (parser
      (stack)
      (lambda ($letter-stack $char)
        (and
          (char-alphabetic? $char)
          (push $letter-stack $char)))
      (lambda ($letter-stack) 
        (and
          (not (null? $letter-stack))
          (string->symbol (list->string (reverse $letter-stack)))))))

  ; ----------------------------------------------------------

  (define (oneof-parser $parsers)
    (lets
      ($push-fns (map parser-push-fn $parsers))
      ($finish-fns (map parser-finish-fn $parsers))
      (parser
        (map parser-state-opt $parsers)
        (lambda ($state-opts $char)
          (map
            (lambda ($state-opt $push-fn)
              (and $state-opt ($push-fn $state-opt $char)))
            $state-opts
            $push-fns))
        (lambda ($state-opts)
          (single
            (filter-opts
              (map
                (lambda ($state-opt $finish-fn)
                  (and $state-opt ($finish-fn $state-opt)))
                $state-opts
                $finish-fns)))))))

  ; ----------------------------------------------------------

  (define indent-size 2)

  (define (indent-parser $parser)
    (lets
      ($state-opt (parser-state-opt $parser))
      ($push-fn (parser-push-fn $parser))
      ($finish-fn (parser-finish-fn $parser))
      (parser
        (cons 0 $state-opt)
        (lambda ($indented $char)
          (lets
            ($indent (car $indented))
            ($body (cdr $indented))
            (case $char
              ((#\space)
                (if (< $indent indent-size)
                  (cons (+ $indent 1) $body)
                  (cons $indent (and $body ($push-fn $body $char)))))
              ((#\newline)
                (and
                  (or (= $indent 0) (= $indent indent-size))
                  (cons 0 (and $body ($push-fn $body $char)))))
              (else 
                (and
                  (= $indent indent-size)
                  (cons (+ $indent 1) (and $body ($push-fn $body $char))))))))
        (lambda ($indented)
          (lets
            ($indent (car $indented))
            ($body (cdr $indented))
            (and (zero? $indent) (and $body ($finish-fn $body))))))))
)