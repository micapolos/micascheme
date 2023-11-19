(library (tico entry)
  (export
    entry entry? entry-parameters entry-arguments
    test-entry
    typings->entry
    entry-let
    entries-let)
  (import
    (micascheme)
    (tico typing)
    (tico datum))

  (data (entry parameters arguments))

  (define-syntax-rule (test-entry $name ...)
    (entry
      (list (test-parameter-typing $name) ...)
      (list (test-typing $name) ...)))

  (define (typings->entry $typings)
    (entry
      (ordered-map typing-parameter $typings)
      $typings))

  (define (entry-let $scope $entry $body)
    (typing-application
      (scope-typing-abstraction
        $scope
        (entry-parameters $entry)
        $body)
      (entry-arguments $entry)))

  (define (entries-let $scope $entries $body)
    (switch $entries
      ((null? _) $body)
      ((pair? $pair)
        (unpair $pair $entry $entries
          (entry-let $scope $entry
            (entries-let
              (fold-left typing-scope-push $scope (entry-parameters $entry))
              $entries
              $body))))))
)
