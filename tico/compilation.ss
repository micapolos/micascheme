(library (tico compilation)
  (export
    compilation compilation? compilation-datum compilation-evaluation

    literal->compilation
    datum->compilation

    compilation-value

    compilation->generate-dependency-opt
    compilation-application)
  (import
    (micascheme)
    (tico constant)
    (tico constant)
    (tico variable)
    (tico dependency)
    (tico packet)
    (tico datum)
    (tico evaluation))

  (data (compilation datum evaluation))

  (define (literal->compilation $literal)
    (compilation $literal (constant $literal)))

  (define (datum->compilation $datum)
    (compilation $datum (datum->constant $datum)))

  (define (compilation-value $compilation)
    (evaluation-value
      (compilation-evaluation $compilation)))

  (define (compilation->generate-dependency-opt $compilation)
    (switch (compilation-evaluation $compilation)
      ((constant? $constant)
        (dependency
          (generate-symbol)
          (packet
            (compilation-datum $compilation)
            (constant-value $constant))))
      ((variable? _) #f)))

  (define (compilation-application $target $args)
    (compilation
      (datum-application
        (compilation-datum $target)
        (map compilation-datum $args))
      (evaluation-application
        (compilation-evaluation $target)
        (map compilation-evaluation $args)
        (lambda ()
          (filter-opts
            (map compilation->generate-dependency-opt
              (reverse (cons $target $args))))))))
)
