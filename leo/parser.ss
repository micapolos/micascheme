(library (leo parser)
  (export
    processor processor? processor-state-opt processor-push-fn processor-finish-fn
    processor-process

    string-processor
    line-processor
    positive-integer-processor
    word-processor
    oneof-processor
    indented-processor

    string-parser string-parser? string-parser-char-stack
    empty-string-parser string-parser-push string-parser-finish

    word-parser word-parser? word-parser-letter-stack
    empty-word-parser word-parser-push word-parser-finish

    positive-integer-parser positive-integer-parser? positive-integer-parser-digit-stack
    empty-positive-integer-parser positive-integer-parser-push positive-integer-parser-finish

    alternatives-parser alternatives-parser? alternatives-parser-alternative-parser-opts
    alternatives-parser-push alternatives-parser-finish

    indented-parser indented-parser? indented-parser-space-count indented-parser-body-parser
    empty-indented-parser indented-parser-push indented-parser-body-parser-opt
    )

  (import (micascheme))

  ; ----------------------------------------------------------

  (data (processor state-opt push-fn finish-fn))

  (define (processor-process $processor $string)
    (lets
      ($state-opt (processor-state-opt $processor))
      ($push-fn (processor-push-fn $processor))
      ($finish-fn (processor-finish-fn $processor))
      ($state-opt
        (fold-left
          (lambda ($state-opt $char)
            (and $state-opt ($push-fn $state-opt $char)))
          $state-opt
          (string->list $string)))
      (and $state-opt ($finish-fn $state-opt))))

  ; ----------------------------------------------------------

  (define (string-processor)
    (processor
      (stack)
      (lambda ($char-stack $char) 
        (push $char-stack $char))
      (lambda ($char-stack) 
        (list->string (reverse $char-stack)))))

  ; ----------------------------------------------------------

  (define (line-processor)
    (processor
      (stack)
      (lambda ($char-stack-or-line $char)
        (and (not (string? $char-stack-or-line))
          (case $char
            ((#\newline) (list->string (reverse $char-stack-or-line)))
            (else (push $char-stack-or-line $char)))))
      (lambda ($char-stack-or-line)
        (and (string? $char-stack-or-line) $char-stack-or-line))))

  ; ----------------------------------------------------------

  (define (positive-integer-processor)
    (processor
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

  (define (word-processor)
    (processor
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

  (define (oneof-processor $processors)
    (lets
      ($push-fns (map processor-push-fn $processors))
      ($finish-fns (map processor-finish-fn $processors))
      (processor
        (map processor-state-opt $processors)
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

  (define (indented-processor $processor)
    (lets
      ($state-opt (processor-state-opt $processor))
      ($push-fn (processor-push-fn $processor))
      ($finish-fn (processor-finish-fn $processor))
      (processor
        (and $state-opt (cons 0 $state-opt))
        (lambda ($indented $char)
          (and $indented
          (lets
            ($indent (car $indented))
            ($body (cdr $indented))
            (and $body
              (case $char
                ((#\space)
                  (if (< $indent indent-size)
                    (cons (+ $indent 1) $body)
                    (cons $indent ($push-fn $body $char))))
                ((#\newline)
                  (and
                    (or (= $indent 0) (= $indent indent-size))
                    (cons 0 ($push-fn $body $char))))
                (else 
                  (and
                    (= $indent indent-size)
                    (cons (+ $indent 1) ($push-fn $body $char)))))))))
        (lambda ($indented)
          (and $indented
            (lets
              ($indent (car $indented))
              ($body (cdr $indented))
              (and (zero? $indent) $body ($finish-fn $body))))))))

  ; ----------------------------------------------------------

  (data (alternatives-parser alternative-parser-opts))

  (define (alternatives-parser-push $alternative-parser-pushes $alternatives-parser $char)
    (alternatives-parser
      (map
        (lambda ($alternative-parser-push $alternative-parser-opt)
          (and $alternative-parser-opt 
            ($alternative-parser-push $alternative-parser-opt $char)))
        $alternative-parser-pushes
        (alternatives-parser-alternative-parser-opts $alternatives-parser))))

  (define (alternatives-parser-finish $alternative-parser-finishes $alternatives-parser)
    (single
      (filter-map
        (lambda ($alternative-parser-finish $alternative-parser-opt)
          (and $alternative-parser-opt 
            ($alternative-parser-finish $alternative-parser-opt)))
        $alternative-parser-finishes
        (alternatives-parser-alternative-parser-opts $alternatives-parser))))

  ; --------------------------------------------------------

  (define indent-space-count 2)

  (data (indented-parser space-count body-parser))

  (define (empty-indented-parser $body-parser) 
    (indented-parser 0 $body-parser))

  (define (indented-parser-push $body-parser-push $indented-parser $char)
    (lets
      ($space-count (indented-parser-space-count $indented-parser))
      ($body-parser (indented-parser-body-parser $indented-parser))
        (case $char
          ((#\space)
            (if (< $space-count indent-space-count)
              (indented-parser 
                (+ $space-count 1) 
                $body-parser)
              (indented-parser $space-count ($body-parser-push $body-parser $char))))
          ((#\newline)
            (and
              (or (= $space-count 0) (= $space-count indent-space-count))
              (indented-parser 0 ($body-parser-push $body-parser $char))))
          (else 
            (and
              (= $space-count indent-space-count)
              (indented-parser (+ $space-count 1) ($body-parser-push $body-parser $char)))))))

  (define (indented-parser-body-parser-opt $indented-parser)
    (and
      (zero? (indented-parser-space-count $indented-parser))
      (indented-parser-body-parser $indented-parser)))

  ; ---------------------------------------------------------

  (data (word-parser letter-stack))

  (define (empty-word-parser)
    (word-parser (stack)))

  (define (word-parser-push $word-parser $char)
    (and
      (char-alphabetic? $char)
      (word-parser (push (word-parser-letter-stack $word-parser) $char))))

  (define (word-parser-finish $word-parser)
    (lets
      ($letter-stack (word-parser-letter-stack $word-parser))
      (and
        (not (null? $letter-stack))
        (string->symbol (list->string (reverse $letter-stack))))))

  ; ---------------------------------------------------------

  (data (string-parser char-stack))

  (define (empty-string-parser)
    (string-parser (stack)))

  (define (string-parser-push $string-parser $char)
    (string-parser (push (string-parser-char-stack $string-parser) $char)))

  (define (string-parser-finish $string-parser)
    (list->string (reverse (string-parser-char-stack $string-parser))))

  ; ---------------------------------------------------------

  (data (positive-integer-parser digit-stack))

  (define (empty-positive-integer-parser)
    (positive-integer-parser (stack)))

  (define (positive-integer-parser-push $positive-integer-parser $char)
    (and
      (char-numeric? $char)
      (positive-integer-parser 
        (push 
          (positive-integer-parser-digit-stack $positive-integer-parser)
          (- (char->integer $char) (char->integer #\0))))))

  (define (positive-integer-parser-finish $positive-integer-parser)
    (lets
      ($digit-stack (positive-integer-parser-digit-stack $positive-integer-parser))
      (and
        (not (null? $digit-stack))
        (fold-left 
          (lambda ($integer $digit) (+ (* $integer 10) $digit))
          0
          (reverse $digit-stack)))))

  ; ---------------------------------------------------------

  (data (empty-rhs-parser body-parser))
  (data (spaced-rhs-parser body-parser))
  (data (indented-rhs-parser indented-body-parser))

  (define (rhs-parser-push $body-parser-push $rhs-parser $char)
    (switch $rhs-parser
      ((empty-rhs-parser? $empty-rhs-parser)
        (case $char
          ((#\space) 
            (spaced-rhs-parser 
              (empty-rhs-parser-body-parser $empty-rhs-parser)))
          ((#\newline) 
            (indented-rhs-parser 
              (empty-indented-parser 
                (empty-rhs-parser-body-parser $empty-rhs-parser))))
          (else #f)))
      ((spaced-rhs-parser? $spaced-rhs-parser)
        (and-lets
          ($body-parser
            ($body-parser-push 
              (spaced-rhs-parser-body-parser $spaced-rhs-parser) 
              $char))
          (spaced-rhs-parser $body-parser)))
      ((indented-rhs-parser $indented-rhs-parser)
        (and-lets
          ($indented-body-parser
            (indented-parser-push
              $body-parser-push
              (indented-rhs-parser-indented-body-parser $indented-rhs-parser) 
              $char))
          (indented-rhs-parser $indented-body-parser)))))
)