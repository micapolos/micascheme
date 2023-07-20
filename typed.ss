(library (typed)
  (export 
    typed typed? typed-value typed-type typed!

    typed-tuple typed-tuple!
    typed-choice!
    typed-function typed-function!
    typed-application typed-application!

    typed-tuple-ref

    option-not option-not? option-not-type
    option-the option-the? option-the-typed
    option-type

    no-selection no-selection?
    multi-selection multi-selection?
    selected selected? selected-index selected-term

    indexed-option-selected
    options-selection

    typed-wrap)

  (import (micascheme) (term) (type))

  (data (typed value type))

  (define-syntax typed!
    (lambda (stx)
      (syntax-case stx ()
        ((_ literal)
          (switch (syntax->datum #`literal)
            ((symbol? $symbol) #`(typed #f (quote literal)))
            ((boolean? $boolean) #`(typed #,$boolean boolean!))
            ((number? $number) #`(typed #,$number number!))
            ((string? $string) #`(typed #,$string string!)))))))

  (define (typed-tuple $name $typed-list)
    (typed
      (case (length $typed-list)
        ((0) #f)
        ((1) (typed-value (car $typed-list)))
        ((2) (cons (typed-value (car $typed-list)) (typed-value (cadr $typed-list))))
        (else (list->vector (map typed-value $typed-list))))
      (tuple-type $name (map typed-type $typed-list))))

  (define-syntax-rule (typed-tuple! ($name $typed ...))
    (typed-tuple (quote $name) (list $typed ...)))

  (define (typed-tuple-ref $typed-tuple $index)
    (lets
      ($tuple-type (typed-type $typed-tuple))
      ($term (typed-value $typed-tuple))
      ($types (tuple-type-types $tuple-type))
      ($type (list-ref $types $index))
      ($size (length $types))
      (typed
        (case $size
          ((1) $term)
          ((2) ((if (= $index 0) pair-first pair-second) $term))
          (else (vector-get $term $index)))
        $type)))

  (define (typed-choice $options)
    (lets
      ($size (length $options))
      ($types (map option-type $options))
      ($selection (options-selection $options))
      (switch $selection
        ((no-selection? _) (throw typed-choice $options `no-selection))
        ((multi-selection? _) (throw typed-choice $options `multi-selection))
        ((selected? $selected)
          (typed
            (pair (selected-index $selected) (selected-term $selected))
            (choice-type $types))))))

  (define-syntax typed-option!
    (syntax-rules (not) 
      ((_ (not type)) (option-not type))
      ((_ typed) (option-the typed))))

  (define-syntax-rule (typed-choice! option ...)
    (typed-choice (list (typed-option! option) ...)))

  (define (typed-function $params $body)
    (typed
      (function (length $params) (typed-value $body))
      (function-type $params (typed-type $body))))

  (define-syntax-rule (typed-function! (param ...) body)
    (typed-function (list param ...) body))

  (define (typed-application $fn $args)
    (typed
      (application (typed-value $fn) (map typed-value $args))
      (function-type-result (typed-type $fn))))

  (define-syntax-rule (typed-application! fn arg ...)
    (typed-application fn (list arg ...)))

  ; ---------------------------------------------------------

  (data (option-not type))
  (data (option-the typed))

  (define (option-type $option)
    (switch $option
      ((option-not? $option-not) (option-not-type $option-not))
      ((option-the? $option-the) (typed-type (option-the-typed $option-the)))))
  
  ; ---------------------------------------------------------

  (data (no-selection))
  (data (selected index term))
  (data (multi-selection))

  (define (indexed-option-selected $indexed-option)
    (switch (indexed-value $indexed-option)
      ((option-the? $option-the)
        (selected
          (indexed-index $indexed-option)
          (typed-value (option-the-typed $option-the))))
      ((option-not? _) #f)
      ((else $other) (throw non-option $other))))

  (define (options-selection $options)
    (lets
      ($indexed-options (list-indexed $options))
      ($selections (filter (lambda (x) x) (map indexed-option-selected $indexed-options)))
      (case (length $selections)
        ((0) (no-selection))
        ((1) (car $selections))
        (else (multi-selection)))))

  ; --------------------------------------------------------

  (define (typed-tuple-ref-index-opt $typed-tuple $index)
    (lets
      ($type (typed-type $typed-tuple))
      ($term (typed-value $typed-tuple))
      ($tuple-types (tuple-type-types $type))
      ($size (length $tuple-types))
      (and (< $index $size)
        (typed
          (case $size
            ((1) $term)
            ((2) ((if (= $index 0) pair-first pair-second) $term))
            (else (vector-get $term $index)))
          (list-ref $tuple-types $index)))))

  ; --------------------------------------------------------

  (define (typed-tuple-ref-symbol-opt $typed-tuple $symbol)
    (lets
      ($type (typed-type $typed-tuple))
      ($term (typed-value $typed-tuple))
      (lets
        ($index (type-selector-index $type $symbol))
        (and $index
          (typed-tuple-ref-index-opt $typed-tuple $index)))))

  ; --------------------------------------------------------

  (define (typed-wrap $typed $to-type)
    (lets
      ($type (typed-type $typed))
      ($term (typed-value $typed))
      (switch $to-type
        ((choice-type? $to-choice-type)
          (and-lets
            ($index (choice-type-index-of $to-choice-type $type))
            (typed (pair $index $term) $to-choice-type)))
        ((tuple-type? $to-tuple-type)
          (and 
            (tuple-type? $type)
            (symbol=? (tuple-type-name $type) (tuple-type-name $to-tuple-type))
            (and-lets 
              ($single-to-type (single (tuple-type-types $to-tuple-type)))
              ($single-type (single (tuple-type-types $type)))
              ($tuple-wrap (typed-wrap $single-type $single-to-type))
              (typed 
                (typed-value $tuple-wrap) 
                (tuple-type 
                  (tuple-type-name $type) 
                  (list (typed-type $tuple-wrap)))))))
        ((else $other) 
          (and 
            (obj=? $type $other) 
            $typed)))))
)
