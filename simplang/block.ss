(library (simplang block)
  (export
    block block?
    block-size block-labels block-equs block-puts
    block-with-size block-with-labels block-with-equs block-with-puts
    empty-block
    block+label block+equ block+data)
  (import (micascheme))

  (data (block size labels equs puts))

  (define (empty-block)
    (block 0 (stack) (stack) (stack)))

  (define (block+label $block $label)
    (block-with-labels $block
      (push
        (block-labels $block)
        (cons $label (block-size $block)))))

  (define (block+equ $block $id $expr)
    (block-with-equs $block
      (push
        (block-equs $block)
        (cons $id $expr))))

  (define (block+data $block $size $put)
    (block-with-puts
      (block-with-size $block (+ (block-size $block) $size))
      (push (block-puts $block) $put)))

  ; (define (block-expr $block $scope $org)
  ;   (lets
  ;     ($labels (reverse (block-labels $block)))
  ;     ($equs (reverse (block-equs $block)))
  ;     `(let
  ;       ())
  ;     ($scope
  ;       (fold-left
  ;         (lambda ($scope $label)
  ;           (push $scope (cons (car $label) 'integer)))
  ;         $scope
  ;         $labels))
  ;     ($scope-exprs
  ;       (fold-left
  ;         (lambda ($scope-exprs $equ)
  ;           (lets
  ;             ($scope (car $scope-exprs))
  ;             ($exprs (cdr $scope-exprs))
  ;             ($id (car $equ))
  ;             ($datum (cdr $equ))
  ;             ($typed (typed $scope $datum))
  ;             ($type (car $typed))
  ;             ($expr (cdr $typed))
  ;             (cons
  ;               (push $scope (cons $id $type))
  ;               (push $exprs $expr))))
  ;         (cons $scope (stack))
  ;         $equs))
  ;     ($scope (car $scope-exprs))
  ;     ($exprs (reverse (cdr $scope-exprs)))
  ;     `(lets
  ;       ,@(map-with ($label $labels)
  ;         `(,(car $label) ,(+ $org (cdr $label))))
  ;       ,@(map-with
  ;         ($equ $equs)
  ;         ($expr $exprs)
  ;         `(,(car $equ) ,$expr)))))
)
