(import
  (micascheme)
  (getter)
  (leo getter))

(pretty-print
  (call-with-input-file "leo/example.leo"
    (lambda ($port)
      (lets
        ((values $lines $bfp $line $column)
          (getter-get! lines-getter
            $port
            (source-file-descriptor "leo/example.leo" 0)
            0 0 0 0))
        $lines))))
