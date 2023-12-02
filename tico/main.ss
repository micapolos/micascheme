(import
  (micascheme)
  (tico tico)
  (tico block)
  (tico typing)
  (tico path)
  (leo parser)
  (leo reader)
  (tico reader))

(display
  (block-string
    (reader-end
      (reader-read-list
        (push-block-reader (stack) (empty-block))
        (load-script
          (path-filename
            (list->path
              (map string->symbol
                (command-line-constants)))))))))
