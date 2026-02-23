(library (procedure-name)
  (export procedure-name?)
  (import (scheme))

  ; Tries to return original procedure name, by parsing it from
  ; its string representation, like "<#procedure +>"
  (define (procedure-name? $proc)
    (let*
      ([$str (format "~a" $proc)]
       [$len (string-length $str)])
    (let find-proc ([$i 0])
      (cond
        ;; Give up if "procedure" can't possibly fit
        [(>= $i (- $len 9)) #f]
        ;; Look for "procedure"
        [(string-match-at? $str $i "procedure")
         (let skip-headers ([$j (+ $i 9)]) ;; Move past "procedure"
           (cond
             [(>= $j $len) 'anonymous-native]
             ;; Skip spaces or the '@' symbol if they exist
             [(or (char=? (string-ref $str $j) #\space)
                  (char=? (string-ref $str $j) #\@))
              (skip-headers (+ $j 1))]
             ;; We are at the start of the name
             [else
              (let find-end ([$k $j])
                (cond
                  [(>= $k $len) (string->symbol (substring $str $j $k))]
                  [else
                   (let ([$c (string-ref $str $k)])
                     ;; The name ends if we hit a space or the closing bracket
                     (if (or (char=? $c #\space) (char=? $c #\>))
                         (let ([$name (substring $str $j $k)])
                           (if (string=? $name "")
                               #f
                               (string->symbol $name)))
                         (find-end (+ $k 1))))]))]))]
        [else (find-proc (+ $i 1))]))))

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
