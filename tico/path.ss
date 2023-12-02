(library (tico path)
  (export
    (rename
      (path! path)
      (path make-path))
    path? path-symbol path-child-path-opt
    list->path
    path-filename
    paths-reader)
  (import
    (micascheme)
    (leo reader))

  (data (path symbol child-path-opt))

  (function (list->path $list)
    (fold-right path #f $list))

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

  (function (path-filename $path)
    (string-append
      (symbol->string (path-symbol $path))
      (switch (path-child-path-opt $path)
        ((path? $path)
          (string-append "/" (path-filename $path)))
        ((false? _) ".leo"))))

  (define-reader (paths-reader $end)
    (fold-reader (stack) push-list
      (lambda ($symbol $end)
        (paths-reader
          (lambda ($paths)
            ($end
              (switch $paths
                ((null? _)
                  (list (path $symbol #f)))
                ((else $paths)
                  (map (partial path $symbol) $paths)))))))
      (lambda ($paths)
        ($end (reverse $paths)))))
)
