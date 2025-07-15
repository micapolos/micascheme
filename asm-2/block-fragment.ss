(library (asm-2 block-fragment)
  (export
    labeled-block-fragment
    fragment->bytevector)
  (import (micascheme) (asm-2 fragment) (asm-2 block))

  (define-case-syntax (labeled-block-fragment line ...)
    (lets
      ($labels
        (filter-opts
          (map-with ($line #'(line ...))
            (syntax-case? $line ()
              (label
                (identifier? #'label)
                #'label)))))
      #`(lets
        #,@(map-with ($label $labels)
          #`(#,$label #f))
        (fragment-map list->block
          (fragment-append
            #,@(map-with ($line #'(line ...))
              (syntax-case $line ()
                (label
                  (identifier? #'label)
                  #'(fragment-with
                    (block 0
                      (lambda ($org)
                        (set! label $org)
                        (empty-binary)))))
                (fragment
                  #'fragment))))))))

  (define (fragment->bytevector $fragment $lookup $org)
    (binary->bytevector
      (block->binary
        (fragment-ref $fragment $lookup)
        $org)))
)
