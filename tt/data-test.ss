(import (tt lang) (tt data))

(define (data point)
  (define (point (number number) point)))

(define (data (pair a b))
  (define (cons a b) (pair a b)))

(define (data (list x))
  (define null (list x))
  (define (link x (list x)) (list x)))

(define (data (term x))
  (define (num number) (term number))
  (define (str string) (term string))
  (define (num+ (term number) (term number)) (term number))
  (define (str+ (term string) (term string)) (term string))
  (define (strlen (term string)) (term number)))
