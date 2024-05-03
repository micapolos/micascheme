(library (transformer)
  (export replace-identifiers)
  (import (scheme))

  (define (replace-identifiers $old $new $syntax)
    (syntax-case $syntax ()
      ($id (identifier? #'$id)
        (if (free-identifier=? #'$id $old) $new #'$id))
      (($x ...)
        #`(
          #,@(map
            (lambda ($syntax)
              (replace-identifiers $old $new $syntax))
            (syntax->list #'($x ...)))))
      ($other #'$other)))
)
