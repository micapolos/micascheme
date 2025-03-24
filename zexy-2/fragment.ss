(library (zexy-2 fragment)
  (export
    sizing sizing? sizing-min sizing-delta sizing-max
    fragment fragment? fragment-timing fragment-blob
    timing timing? timing-min timing-max

    sizing+
    list->sizing
    list->timing
    list->fragment)
  (import (micascheme))

  (data (sizing min delta max))
  (data (timing min max))
  (data (fragment sizing timing blob))

  (define (sizing+ $a $b)
    (lets
      ($delta (+ (sizing-delta $a) (sizing-delta $b)))
      (sizing
        (min (- (sizing-min $a) $delta) (sizing-min $b))
        $delta
        (max (- (sizing-max $a) $delta) (sizing-max $b)))))

  (define (list->sizing $sizings)
    (fold-left sizing+ (sizing 0 0 0) $sizings))

  (define (list->timing $timings)
    (timing
      (apply max (map timing-min $timings))
      (apply max (map timing-max $timings))))

  (define (list->fragment $fragments)
    (fragment
      (list->sizing (map fragment-sizing $fragments))
      (list->timing (map fragment-timing $fragments))
      (list->blob (map fragment-blob $fragments))))
)
