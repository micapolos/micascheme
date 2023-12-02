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

  (function (empty-block)
    (block (stack) (stack)))

  (function (block-update-entries $block $fn)
    (block
      ($fn (block-entries $block))
      (block-typings $block)))

  (function (block+entry $block $entry)
    (block-update-entries $block
      (lambda ($entries)
        (push $entries $entry))))

  (function (block-update-typings $block $fn)
    (block
      (block-entries $block)
      ($fn (block-typings $block))))

  (function (block-end-typings $block)
    (run
      (ensure null? (block-entries $block))
      (block-typings $block)))

  (function (block-typing $block)
    (or-throw (single (block-typings $block))))

  (function (block+typing $block $typing)
    (block-update-typings $block
      (lambda ($typings)
        (push $typings $typing))))

  (function (block+typings $block $typings)
    (block-update-typings $block
      (lambda ($block-typings)
        (push-list $block-typings $typings))))

  (function (block-with-typing $block $typing)
    (block-update-typings $block
      (lambda (_)
        (stack $typing))))

  (function (block-with-typings $block $typings)
    (block-update-typings $block
      (lambda (_)
        $typings)))

  (function (block-let $scope $block $body-fn)
    (entries-let
      $scope
      (block-entries $block)
      (lambda ($scope)
        ($body-fn $scope
          (block-typings $block)))))

  (function (block-struct $name $block)
    (block-let
      (empty-stack-typing)
      $block
      (lambda ($scope $typings)
        (typing-struct $name (reverse $typings)))))

  (function (block-string $block)
    (typings-string (reverse (block-typings $block))))
)
