(import
  (micascheme)
  (tico tico)
  (tico typing)
  (tico path)
  (leo parser)
  (leo reader)
  (tico reader))

(display
  (args-typing-string
    (reader-end
      (reader-read-list
        (args-typing-reader (stack))
        (load-script
          (path-filename
            (list->path
              (map string->symbol
                (command-line-arguments)))))))))
