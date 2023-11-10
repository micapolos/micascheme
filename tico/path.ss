(library (tico path)
  (export
    (rename
      (path! path)
      (path make-path))
    path? path-symbol path-child-path-opt
    path-filename)
  (import
    (micascheme))

  (data (path symbol child-path-opt))

  (define-syntax path!
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $name)
          (identifier? #'$name)
          #'(path (quote $name) #f))
        ((_ $name $name* ...)
          (identifier? #'$name)
          #'(path (quote $name)
            (path! $name* ...))))))

  (define (path-filename $path)
    (string-append
      (symbol->string (path-symbol $path))
      (switch (path-child-path-opt $path)
        ((path? $path)
          (string-append "/" (path-filename $path)))
        ((false? _) ".leo"))))
)
