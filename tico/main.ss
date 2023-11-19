(import
  (micascheme)
  (tico tico)
  (tico typing)
  (tico path)
  (leo parser)
  (leo reader)
  (tico reader))

(display
  (typing-string
    (reader-end
      (reader-read-list
        (typing-reader (stack))
        (load-script
          (path-filename
            (list->path
              (map string->symbol
                (command-line-arguments)))))))))
