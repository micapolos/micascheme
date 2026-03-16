import
  leo scheme
  language
  leo language
  leo examples math

define-language
  printer
  language
    list "print"
    make-read-lambda
      with: port, sfd, bfp
      switch
        get-line port
        when
          eof? eof
          values: eof, bfp
        when
          any? line
          values
            string-append: number->string bfp, ": ", line
            add: bfp, string-length line, 1
    lambda
      with: datum, list args
      displayln datum
      apply: sc-expand, #f, args

printer load "leo/examples/file.print"
