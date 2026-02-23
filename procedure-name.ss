(library (procedure-name)
  (export procedure-name?)
  (import (scheme))

  ;; Tries to return original procedure name by parsing its
  ;; string representation, filtering out location metadata.
  (define (procedure-name? $proc)
    (let* ([$str (format "~a" $proc)]
           [$len (string-length $str)])
      (let find-proc ([$i 0])
        (cond
          [(>= $i (- $len 9)) #f]
          [(string-match-at? $str $i "procedure")
           (let skip-headers ([$j (+ $i 9)])
             (cond
               [(>= $j $len) #f]
               [(or (char=? (string-ref $str $j) #\space)
                    (char=? (string-ref $str $j) #\@))
                (skip-headers (+ $j 1))]
               [else
                (let find-end ([$k $j])
                  (cond
                    [(>= $k $len) (handle-name (substring $str $j $k))]
                    [else
                     (let ([$c (string-ref $str $k)])
                       (if (or (char=? $c #\space) (char=? $c #\>))
                           (handle-name (substring $str $j $k))
                           (find-end (+ $k 1))))]))]))]
          [else (find-proc (+ $i 1))]))))

  ;; Private helper to distinguish names from location markers
  (define (handle-name $name)
    (cond
      [(string=? $name "") #f]
      [(string=? $name "at") #f] ;; 'at' indicates a location follows, not a name
      [else (string->symbol $name)]))

  (define (string-match-at? $str $pos $pat)
    (let ([$pat-len (string-length $pat)])
      (let loop ([$i 0])
        (cond
          [(= $i $pat-len) #t]
          [(>= (+ $pos $i) (string-length $str)) #f]
          [(char=? (string-ref $str (+ $pos $i)) (string-ref $pat $i))
           (loop (+ $i 1))]
          [else #f]))))
)
