(library (zexy res)
  (export)
  (import
    (micascheme)
    (zexy bin))

  (data (res stack org labels))

  (define (res-value $res $value)
    (res
      (push (res-stack $res) $value)
      (res-org $res)
      (res-labels $res)))

  (define (res-org-nm $res $nm)
    (res
      (res-stack $res)
      $nm
      (res-labels $res)))

  (define (res-label $res $label)
    (res
      (res-stack $res)
      (res-org $res)
      (push (res-labels $res) (cons $label (res-org $res)))))

  (define (res-op $res $syntax)
    $res)
)
