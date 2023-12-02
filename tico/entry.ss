(library (tico entry)
  (export
    entry entry? entry-parameters entry-constants
    test-entry
    typings->entry
    entry-let
    entries-let)
  (import
    (micascheme)
    (tico arity)
    (tico typing)
    (tico datum)
    (tico layment)
    (tico compilation))

  (data (entry parameters constants))

  (define-syntax-rule (test-entry $name ...)
    (entry
      (list (test-parameter-typing $name) ...)
      (list (test-typing $name) ...)))

  (define (typings->entry $typings)
    (entry
      (ordered-map typing-parameter $typings)
      $typings))

  (define (entry-let-entries-datum $entry)
    (map
      (lambda ($param $arg)
        `(
          ,(typing-datum $param)
          ,(typing-datum $arg)))
      (entry-parameters $entry)
      (entry-constants $entry)))

  (define (entry-let $scope $entry $body-fn)
    (lets
      ($body-typing
        ($body-fn
          (fold-left
            stack-typing-push
            $scope
            (entry-parameters $entry))))
      ($body-layment (typing-layment $body-typing))
      ($body-compilation (layment-compilation $body-layment))
      (typing
        (typing-type $body-typing)
        (make-layment
          (layment-layout $body-layment)
          (compilation
            (arity 1)
            (let-datum
              (entry-let-entries-datum $entry)
              (compilation-datum $body-compilation))
            (compilation-evaluation $body-compilation))))))

  (define (entries-let $scope $entries $body-fn)
    (switch $entries
      ((null? _)
        ($body-fn $scope))
      ((pair? $pair)
        (unpair $pair $entry $entries
          (entry-let $scope $entry
            (lambda ($scope)
              (entries-let
                $scope
                $entries
                $body-fn)))))))
)
