(library (tico block)
  (export
    block block? block-entries block-typings
    empty-block
    block-end-typings
    block-update-entries
    block-update-typings
    block+entry
    block-typing
    block-with-typing
    block-with-typings
    block+entry
    block+typing
    block+typings
    block-let
    block-struct
    block-string)
  (import
    (micascheme)
    (tico binding)
    (tico typing)
    (tico entry))

  (data (block entries typings))

  (define (empty-block)
    (block (stack) (stack)))

  (define (block-update-entries $block $fn)
    (block
      ($fn (block-entries $block))
      (block-typings $block)))

  (define (block+entry $block $entry)
    (block-update-entries $block
      (lambda ($entries)
        (push $entries $entry))))

  (define (block-update-typings $block $fn)
    (block
      (block-entries $block)
      ($fn (block-typings $block))))

  (define (block-end-typings $block)
    (ensure null? (block-entries $block))
    (block-typings $block))

  (define (block-typing $block)
    (or-throw (single (block-typings $block))))

  (define (block+typing $block $typing)
    (block-update-typings $block
      (lambda ($typings)
        (push $typings $typing))))

  (define (block+typings $block $typings)
    (block-update-typings $block
      (lambda ($block-typings)
        (push-list $block-typings $typings))))

  (define (block-with-typing $block $typing)
    (block-update-typings $block
      (lambda (_)
        (stack $typing))))

  (define (block-with-typings $block $typings)
    (block-update-typings $block
      (lambda (_)
        $typings)))

  (define (block-let $scope $block $body-fn)
    (entries-let
      $scope
      (block-entries $block)
      (lambda ($scope)
        ($body-fn $scope
          (block-typings $block)))))

  (define (block-struct $name $block)
    (block-let
      (empty-typing-scope)
      $block
      (lambda ($scope $typings)
        (typing-struct $name (reverse $typings)))))

  (define (block-string $block)
    (typings-string (reverse (block-typings $block))))
)
