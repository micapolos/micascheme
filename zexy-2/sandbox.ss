(import
  (micascheme)
  (zexy-2 lang)
  (zexy-2 fragment)
  (zexy math)
  (zexy-2 syntax)
  (zexy-2 blob)
  (zexy-2 keywords)
  (zexy-2 value))

(define-fragment ret
  (lambda ($lookup $syntax)
    (syntax-case $syntax ()
      ((_)
        (fragment
          (sizing 0 2)
          (timing 10 10)
          (blob #xc9))))))

(define-fragment call
  (lambda ($lookup $syntax)
    (syntax-case $syntax ()
      ((_ nm)
        (lets
          ($nm (syntax->u16 $lookup #'nm))
          (fragment
            (sizing 2 0)
            (timing 17 17)
            (blob-append
              (u8-blob #xcd)
              (u16-blob $nm)))))
      ((_ c nm)
        (lets
          ($c (c-syntax->u3 #'c))
          ($nm (syntax->u16 $lookup #'nm))
          (fragment
            (sizing 2 0)
            (timing 10 17)
            (blob-append
              (u2-u3-u3-blob #b11 $c #b100)
              (u16-blob $nm))))))))
