(import
  (micascheme)
  (tico tico)
  (tico path))

(writeln
  (tico-load
    (path-filename
      (list->path
        (map string->symbol
          (command-line-arguments))))))

