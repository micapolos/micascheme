(library (parser)
  (export
    parse-error parse-error? parse-error-line parse-error-column

    parser
    parser parser-bind parser-map
    parser-lets
    parse

    space-parser
    newline-parser
    newline-ended-parser

    opt-parser
    char-parser-bind
    exact-char-parser
    char-parser
    string-parser
    digit-parser
    letter-parser
    exact-parser
    positive-integer-parser
    word-parser
    oneof-parser
    fold-parser
    stack-parser
    separator-stack-parser
    non-empty-stack-parser
    indent-parser

    pure skip selected

    string-literal-char-parser
    literal-string-parser
  )

  (import (micascheme))

  ; ----------------------------------------------------------

  (define-aux-keyword selected)
  (define-aux-keyword pure)
  (define-aux-keyword skip)

  ; ----------------------------------------------------------

  (data (parsed value))
  (data (thunk parsed-opt push-fn))

  (define (thunk-push $thunk $char)
    ((thunk-push-fn $thunk) $char))

  ; ----------------------------------------------------------

  (define-syntax-rule (parser-thunk-do ($thunk $parser) $body)
    (and-lets ($thunk $parser) $body))

  (define (parser-push $parser $char)
    (parser-thunk-do ($thunk $parser)
      (thunk-push $thunk $char)))

  (define (parser-parsed-opt $parser)
    (parser-thunk-do ($thunk $parser)
      (thunk-parsed-opt $thunk)))

  (define (parser $value)
    (thunk (parsed $value) (lambda ($char) #f)))

  (define (parser-bind-with $parser $fn-parser $fn)
    (cond
      ((not $parser) $fn-parser)
      (else
        (lets
          ($thunk $parser)
          ($parsed-opt (thunk-parsed-opt $thunk))
          ($fn-parser (or (and $parsed-opt ($fn (parsed-value $parsed-opt))) $fn-parser))
          (thunk
            (parser-parsed-opt $fn-parser)
            (lambda ($char)
              (parser-bind-with
                (thunk-push $thunk $char)
                (parser-push $fn-parser $char)
                $fn)))))))

  (define (parser-bind $parser $fn)
    (parser-bind-with $parser #f $fn))

  (define (parser-map $parser $fn)
    (parser-bind $parser 
      (lambda ($item) 
        (parser ($fn $item)))))

  ; ----------------------------------------------------------

  (define-syntax parser-lets
    (lambda ($syntax)
      (syntax-case $syntax (skip parser)
        ((_ (skip $expr) $decl ... $body)
          #`(parser-bind $expr
            (lambda (_)
              (parser-lets $decl ... $body))))
        ((_ ($var (parser $expr)) $decl ... $body)
          #`(lets ($var $expr)
            (parser-lets $decl ... $body)))
        ((_ ($var $expr) $decl ... $body)
          #`(parser-bind $expr
            (lambda ($var)
              (parser-lets $decl ... $body))))
        ((_ (parser $body)) 
          #`(parser $body))
        ((_ $body)
          #`$body))))

  ; ----------------------------------------------------------

  (data (parse-error line column))

  (define (parse-from $parser $string $index $line $column)
    (parser-thunk-do ($thunk $parser)
      (cond
        ((= $index (string-length $string))
          (lets
            ($parsed-opt (thunk-parsed-opt $thunk))
            (if $parsed-opt
              (parsed-value $parsed-opt)
              (parse-error $line $column))))
        (else
          (lets
            ($char (string-ref $string $index))
            ($parser (thunk-push $thunk $char))
            (if (not $parser)
              (parse-error $line $column)
              (parse-from
                $parser
                $string
                (+ $index 1)
                (if (char=? $char #\newline) (+ $line 1) $line)
                (if (char=? $char #\newline) 1 (+ $column 1)))))))))

  (define (parse $parser $string)
    (parse-from $parser $string 0 1 1))

  ; ----------------------------------------------------------

  (define (opt-parser $parser)
    (oneof-parser (parser #f) $parser))

  ; ----------------------------------------------------------

  (define-syntax-rule (char-parser-bind $char $body)
    (thunk #f (lambda ($char) $body)))

  (define (exact-char-parser $exact-char)
    (char-parser-bind $char
      (and 
        (char=? $char $exact-char) 
        (parser $char))))

  (define (char-parser)
    (char-parser-bind $char
      (parser $char)))

  ; ----------------------------------------------------------

  (define (make-string-parser $char-stack)
    (thunk
      (parsed (list->string (reverse $char-stack)))
      (lambda ($char)
        (make-string-parser (push $char-stack $char)))))

  (define (string-parser)
    (make-string-parser (stack)))

  ; ----------------------------------------------------------

  (define (make-exact-parser $string $index)
    (thunk
      (and
        (= $index (string-length $string))
        (parsed $string))
      (lambda ($char)
        (and 
          (< $index (string-length $string))
          (char=? $char (string-ref $string $index))
          (make-exact-parser $string (+ $index 1))))))

  (define (exact-parser $string)
    (make-exact-parser $string 0))

  ; ----------------------------------------------------------

  (define (space-parser) (exact-char-parser #\space))
  (define (newline-parser) (exact-char-parser #\newline))

  (define (newline-ended-parser $parser)
    (parser-lets
      ($value $parser)
      (skip (newline-parser))
      (parser $value)))

  ; ----------------------------------------------------------

  (define (digit-parser)
    (char-parser-bind $char
      (and
        (char-numeric? $char)
        (parser (- (char->integer $char) (char->integer #\0))))))

  ; ----------------------------------------------------------

  (define (positive-integer-parser)
    (parser-lets
      ($digit-stack (non-empty-stack-parser (digit-parser)))
      (parser 
        (fold-left 
          (lambda ($integer $digit) (+ (* $integer 10) $digit))
          0
          (reverse $digit-stack)))))

  ; ----------------------------------------------------------

  (define (letter-parser)
    (char-parser-bind $char
      (and 
        (char-alphabetic? $char)
        (parser $char))))

  ; ----------------------------------------------------------
  
  (define (word-parser)
    (parser-lets
      ($letter-stack (non-empty-stack-parser (letter-parser)))
      (parser (string->symbol (list->string (reverse $letter-stack))))))

  ; ----------------------------------------------------------

  (define (make-oneof-parser $parsers $else)
    (lets
      ($thunks (filter-opts $parsers))
      (case (length $thunks)
        ((0) $else)
        ((1) (car $thunks))
        (else 
          (thunk
            (lets
              ($values (map parsed-value (filter-opts (map thunk-parsed-opt $thunks))))
              (case (length $values)
                ((0) (parser-parsed-opt $else))
                ((1) (parsed (car $values)))
                (else #f)))
            (lambda ($char)
              (make-oneof-parser
                (map 
                  (lambda ($thunk) 
                    (thunk-push $thunk $char))
                  $thunks)
                (parser-push $else $char))))))))

  (define-syntax oneof-parser
    (lambda ($syntax)
      (syntax-case $syntax (else)
        ((_ $parser ... (else $else))
          #`(make-oneof-parser (list $parser ...) $else))
        ((_ $parser ...)
          #`(oneof-parser $parser ... (else #f))))))

  ; ----------------------------------------------------------

  (define (fold-parser $folded $parser $fn)
    (oneof-parser
      (parser $folded)
      (parser-bind $parser
        (lambda ($item)
          (fold-parser ($fn $folded $item) $parser $fn)))))

  (define (stack-parser $parser)
    (fold-parser (stack) $parser push))

  (define (non-empty-stack-parser $parser)
    (parser-lets
      ($first $parser)
      (fold-parser (stack $first) $parser push)))

  (define (separator-stack-parser $item-parser $separator-parser)
    (oneof-parser
      (parser (stack))
      (parser-lets
        ($first $item-parser)
        (fold-parser
          (stack $first)
          (parser-lets (skip $separator-parser) $item-parser)
          push))))

  ; ----------------------------------------------------------

  (define indent-size 2)

  (define (make-indent-parser $indent $parser)
    (thunk
      (and
        (= $indent indent-size)
        (parser-parsed-opt $parser))
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
                (parser-push $parser $char))))))))

  (define (indent-parser $parser)
    (make-indent-parser 0 $parser))

  ; ----------------------------------------------------------

  (define (string-literal-char-parser)
    (char-parser-bind $char
      (case $char
        ((#\\)
          (char-parser-bind $char
            (case $char
              ((#\\) (parser #\\))
              ((#\n) (parser #\newline))
              ((#\t) (parser #\tab))
              ((#\") (parser #\"))
              (else #f))))
        ((#\") #f)
        (else (parser $char)))))

  (define (string-literal-body-parser)
    (parser-lets
      ($char-stack (stack-parser (string-literal-char-parser)))
      (parser (list->string (reverse $char-stack)))))

  (define (literal-string-parser)
    (parser-lets
      (skip (exact-parser "\""))
      ($string (string-literal-body-parser))
      (skip (exact-parser "\""))
      (parser $string)))
)
