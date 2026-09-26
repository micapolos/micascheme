(import
  (scheme)
  (only (mica reader) read-file)
  (leo3 reader line))

(pretty-print (annotation-stripped (read-file line-annotation "tata-8/game.leo")))
