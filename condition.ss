(library (condition)
  (export condition->datum)
  (import
    (scheme)
    (switch)
    (lets)
    (procedure)
    (lets)
    (list))

  (define (syntax->condition-datums $syntax)
    (switch (syntax->annotation $syntax)
      ((annotation? $annotation)
        (lets
          ($source (annotation-source $annotation))
          ((values $path $line $column) (locate-source-object-source $source #t #t))
          `(
            ,(syntax->datum $syntax)
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
        `(message
          ,(if (exists format-condition? $simple-conditions)
            (lets
              ($message-condition (car (memp message-condition? $simple-conditions)))
              ($irritants-condition (car (memp irritants-condition? $simple-conditions)))
              (apply format
                (condition-message $message-condition)
                (condition-irritants $irritants-condition)))
            (condition-message $message-condition))))
      ((syntax-violation? $syntax-violation)
        `(syntax-violation
          (form ,@(syntax->condition-datums (syntax-violation-form $syntax-violation)))
          (subform ,@(syntax->condition-datums (syntax-violation-subform $syntax-violation)))))
      ((source-condition? $source-condition)
        `(source ,@(syntax->condition-datums (source-condition-form $source-condition))))
      ((serious-condition? _) `serious)
      ((assertion-violation? _) `assertion-violation)
      ((undefined-violation? _) `undefined-violation)
      ((error? _) `error)
      ((warning? _) `warning)
      ((lexical-violation? _) `lexical-violation)
      ((i/o-error? _) `i/o-error)
      ((i/o-write-error? _) `i/o-write-error)
      ((else _) #f)))
)
