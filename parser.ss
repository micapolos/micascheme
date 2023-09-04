(library (parser)
  (export
    parse-error parse-error? parse-error-line parse-error-column

    parser
    parser-push parser-finish
    parser parser-bind parser-map
    parser-lets
    parse

    make-parser
    define-parser

    bind-char-parser
    exact-char-parser
    select-parser
    char-parser
    string-parser
    digit-parser
    letter-parser
    exact-parser
    line-parser
    positive-integer-parser
    word-parser
    oneof-parser
    fold-parser
    stack-parser
    non-empty-stack-parser
    indent-parser

    parsed pure skip selected

    string-literal-char-parser
    literal-string-parser
  )

  (import (micascheme))

  ; ----------------------------------------------------------

  (define-aux-keyword parsed)
  (define-aux-keyword selected)
  (define-aux-keyword pure)
  (define-aux-keyword skip)

  ; ----------------------------------------------------------

  (data (thunk push-fn finish-fn))

  (define (thunk-push $thunk $char)
    ((thunk-push-fn $thunk) $char))

  (define (thunk-finish $thunk)
    ((thunk-finish-fn $thunk)))

  ; ----------------------------------------------------------

  (define-syntax-rule (parser-thunk-do ($thunk $parser) $body)
    (and-lets ($thunk $parser) $body))

  (define (parser-push $parser $char)
    (parser-thunk-do ($thunk $parser)
      (thunk-push $thunk $char)))

  (define (parser-finish $parser)
    (parser-thunk-do ($thunk $parser)
      (thunk-finish $thunk)))

  (define (parser $value)
    (thunk 
      (lambda ($char) #f) 
      (lambda () $value)))

  (define (parser-bind $parser $fn)
    (parser-thunk-do ($thunk $parser)
      (thunk
        (lambda ($char)
          (or
            (parser-bind (thunk-push $thunk $char) $fn)
            (parser-push (opt-lift $fn (thunk-finish $thunk)) $char)))
        (lambda ()
          (parser-finish
            (opt-lift $fn (thunk-finish $thunk)))))))

  (define (parser-map $parser $fn)
    (parser-bind $parser 
      (lambda ($item) 
        (parser ($fn $item)))))

  ; ----------------------------------------------------------

  (define-syntax select-parser
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $item ...)
          (lets
            ($items
              (map
                (lambda ($item)
                  (syntax-case $item (selected)
                    ((selected $item) (cons #t #`$item))
                    ($item (cons #f #`$item))))
                (syntax->list #`($item ...))))
            ($selected
              (single
                (filter 
                  (lambda ($item)
                    (and (car $item) (cdr $item)))
                  $items)))
            (if (not $selected)
              (syntax-error $syntax "nothing selected")
              (lets
                ($var (generate-temporary #`selected))
                #`(parser-lets
                  #,@(map
                    (lambda ($item)
                      #`(
                        #,(if (car $item) $var #`skip)
                        #,(cdr $item)))
                    $items)
                  (parser #,$var)))))))))

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

  (meta define (parser-identifier $id)
    (build-identifier ($string $id) 
      (string-append $string "-parser")))

  (define-syntax make-parser
    (lambda ($syntax)
      (syntax-case $syntax (skip pure parsed lets)
        ((_ (pure $body))
          #`$body)
        ((_ (parsed $body)) 
          #`(parser $body))
        ((_ (lets $body))
          #`(make-parser $body))
        ((_ (lets (skip $expr) $decl ... $body))
          #`(parser-bind 
            (make-parser $expr)
            (lambda (_)
              (make-parser (lets $decl ... $body)))))
        ((_ (lets ($var $expr) $decl ... $body))
          #`(parser-bind
            (make-parser $expr)
            (lambda ($var)
              (make-parser (lets $decl ... $body)))))
        ((_ ($id $arg ...)) (identifier? #`$id)
          #`(
            #,(parser-identifier #`$id)
            #,@(map
              (lambda ($arg) #`(make-parser #,$arg))
              (syntax->list #`($arg ...)))))
        ((_ $id) (identifier? #`$id)
          #`(make-parser ($id)))
        ((_ $constant)
          (switch (syntax->datum #`$constant)
            ((string? $string) #`(exact-parser (quote #,$string)))
            ((else $other) $other))))))

  (define-syntax define-parser
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ ($name $arg ...) $body) (identifier? #`$name)
          #`(define (#,(parser-identifier #`$name) $arg ...) 
            (make-parser $body)))
        ((_ $name $body)
          #`(define-parser ($name) $body)))))

  ; ----------------------------------------------------------

  (data (parse-error line column))

  (define (parse-from $parser $string $index $line $column)
    (parser-thunk-do ($thunk $parser)
      (cond
        ((= $index (string-length $string))
          (or
            (thunk-finish $thunk)
            (parse-error $line $column)))
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

  (define-syntax-rule (bind-char-parser $char $body)
    (thunk 
      (lambda ($char) $body)
      (lambda () #f)))

  (define (exact-char-parser $exact-char)
    (bind-char-parser $char
      (and 
        (char=? $char $exact-char) 
        (parser $char))))

  (define (char-parser)
    (bind-char-parser $char
      (parser $char)))

  ; ----------------------------------------------------------

  (define (make-string-parser $char-stack)
    (thunk
      (lambda ($char)
        (make-string-parser (push $char-stack $char)))
      (lambda ()
        (list->string (reverse $char-stack)))))

  (define (string-parser)
    (make-string-parser (stack)))

  ; ----------------------------------------------------------

  (define (make-exact-parser $string $index)
    (thunk
      (lambda ($char)
        (and 
          (< $index (string-length $string))
          (char=? $char (string-ref $string $index))
          (make-exact-parser $string (+ $index 1))))
      (lambda ()
        (and
          (= $index (string-length $string))
          $string))))

  (define (exact-parser $string)
    (make-exact-parser $string 0))

  ; ----------------------------------------------------------

  (define (make-line-parser $char-stack-or-line)
    (thunk
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

  (define (digit-parser)
    (bind-char-parser $char
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
    (bind-char-parser $char
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
            (lambda ($char)
              (make-oneof-parser
                (map 
                  (lambda ($thunk) 
                    (thunk-push $thunk $char))
                  $thunks)
                (parser-push $else $char)))
            (lambda ()
              (lets
                ($values
                  (filter-opts 
                    (map thunk-finish $thunks)))
                (case (length $values)
                  ((0) (parser-finish $else))
                  ((1) (car $values))
                  (else #f)))))))))

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

  ; ----------------------------------------------------------

  (define indent-size 2)

  (define (make-indent-parser $indent $parser)
    (thunk
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

  ; ----------------------------------------------------------

  (define (string-literal-char-parser)
    (bind-char-parser $char
      (case $char
        ((#\\)
          (bind-char-parser $char
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
    (select-parser
      (exact-parser "\"")
      (selected (string-literal-body-parser))
      (exact-parser "\"")))
)