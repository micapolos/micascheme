(library (condition)
  (export
    &cause make-cause-condition cause-condition? condition-cause
    &hint make-hint-condition hint-condition? condition-hint
    condition->datum)
  (import
    (scheme)
    (switch)
    (lets)
    (procedure)
    (boolean)
    (lets)
    (system)
    (list))

  (define-condition-type &cause &condition make-cause-condition cause-condition?
    (cause condition-cause))

  (define-condition-type &hint &condition make-hint-condition hint-condition?
    (hint condition-hint))

  (define (datum-simplify $datum)
    (switch $datum
      ((pair? $pair)
        `(
          ,(car $datum)
          ,@(map* datum-simplify-inner datum-simplify-inner (cdr $datum))))
      ((else $other) $other)))

  (define (datum-simplify-inner $datum)
    (switch $datum
      ((pair? $pair)
        `(,(car $datum) ...))
      ((else $other) $other)))

  (define (syntax->condition-datums $syntax)
    (switch (syntax->annotation $syntax)
      ((annotation? $annotation)
        (lets
          ($source (annotation-source $annotation))
          ((values $path $line $column) (locate-source-object-source $source #t #t))
          `(
            ,(datum-simplify (syntax->datum $syntax))
            (in ,$path)
            (at
              (line ,$line)
              (column ,$column)))))
      ((else _)
        (list (syntax->datum $syntax)))))

  (define (condition->datum $condition)
    (lets
      ($simple-conditions (simple-conditions $condition))
      (case (length $simple-conditions)
        ((1) (simple-condition->datum? $simple-conditions (car $simple-conditions)))
        (else
          `(condition
            ,@(?filter
              (map
                (partial simple-condition->datum? $simple-conditions)
                $simple-conditions)))))))

  (define (simple-condition->datum? $simple-conditions $condition)
    (switch $condition
      ((i/o-invalid-position-error? $i/o-error)
        `(i/o-invalid-position ,(i/o-error-position $i/o-error)))
      ((i/o-filename-error? $i/o-error)
        `(i/o-filename-error ,(i/o-error-filename $i/o-error)))
      ((who-condition? $who-condition)
        `(who ,(condition-who $who-condition)))
      ((message-condition? $message-condition)
        (if (exists format-condition? $simple-conditions)
          (lets
            ($message-condition (car (memp message-condition? $simple-conditions)))
            ($irritants-condition (car (memp irritants-condition? $simple-conditions)))
            ($message (condition-message $message-condition))
            ($irritants (condition-irritants $irritants-condition))
            (case $message
              (("variable ~:s is not bound")
                `(unbound (variable ,(car $irritants))))
              (else
                `(message
                  ,(apply format
                    (condition-message $message-condition)
                    (condition-irritants $irritants-condition))))))
          `(message ,(condition-message $message-condition))))
      ((syntax-violation? $syntax-violation)
        `(syntax-violation
          ,@(syntax->condition-datums (syntax-violation-form $syntax-violation))
          ,@(switch (syntax-violation-subform $syntax-violation)
            ((false? _) '())
            ((else $subform)
              (list `(subform ,@(syntax->condition-datums $subform)))))))
      ((cause-condition? $cause-condition)
        `(cause ,(condition-cause $cause-condition)))
      ((hint-condition? $hint-condition)
        `(hint ,(condition-hint $hint-condition)))
      ((source-condition? $source-condition)
        `(source ,@(syntax->condition-datums (source-condition-form $source-condition))))
      ((i/o-error? _) `i/o-error)
      ((i/o-write-error? _) `i/o-write-error)
      ((lexical-violation? _) `lexical-violation)
      ((assertion-violation? _) `assertion-violation)
      ((serious-condition? _) `serious)
      ((undefined-violation? _) `undefined-violation)
      ((error? _) `error)
      ((warning? _) `warning)
      ((else _) #f)))
)
