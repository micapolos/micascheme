(library (asm-2 asm)
  (export asm)
  (import (micascheme) (asm-2 fragment) (asm-2 block))

  (define-case-syntax (asm line ...)
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

)
