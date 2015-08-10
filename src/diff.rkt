#lang racket

(provide (all-defined-out))

(define (xterm-colorize color str)
  (define str-color
    (case color
      [(none) "0"]
      [(black) "0;30"]
      [(red) "0;31"]
      [(green) "0;32"]
      [(brown) "0;33"]
      [(blue) "0;34"]
      [(purple) "0;35"]
      [(cyan) "0;36"]
      [(light-gray) "0;37"]
      [(dark-grey) "1:30"]
      [(light-red) "1;31"]
      [(light-green) "1;32"]
      [(yellow) "1;33"]
      [(light-blue) "1;34"]
      [(light-purple) "1;35"]
      [(light-cyan) "1;36"]
      [(white) "1;37"]
      [else (error 'xterm-color "invalid color ~a" color)]))
  (format "\033[~am~a\033[0m" str-color str))

(define-struct color (expr)
               #:property prop:custom-write
               (lambda (color port write?) 
                 (display (xterm-colorize 'red (pretty-format (color-expr color)))
                          port)))

;; first n elements of a list
(define (first-n lst n)
  (reverse
    (for/fold ([out '()])
              ([i (in-range 0 n)]
               [expr (in-list lst)])
              (cons expr out))))

;; take off the first n elements of a list
(define (rest-n lst n)
  (let loop ([out lst]
             [i 0])
    (if (= i n)
      out
      (loop (cdr out) (add1 i)))))

(define (get-differences expr1 expr2)
  (let ([len1 (if (list? expr1) (length expr1) 0)]
        [len2 (if (list? expr2) (length expr2) 0)])
    (cond
      [(and (list? expr1)
            (list? expr2)
            (not (= len1 len2)))
       (if (> len1 len2)
         (let-values ([(r1 r2)
                       (get-differences (first-n expr1 len2)
                                        expr2)])
           (values (append r1 (map make-color (rest-n expr1 len2)))
                   r2))
         (let-values ([(r1 r2)
                       (get-differences expr1
                                        (first-n expr2 len1))])
           (values r1
                   (append r2 (map make-color (rest-n expr2 len1))))))]
      [(and (list? expr1)
            (list? expr2))
       (let-values ([(out1 out2)
                     (for/fold ([result1 '()]
                                [result2 '()])
                               ([i1 (in-list expr1)]
                                [i2 (in-list expr2)])
                               (let-values ([(r1 r2)
                                             (get-differences i1 i2)])
                                 (values (cons r1 result1)
                                         (cons r2 result2))))])
         (values (reverse out1) (reverse out2)))]
      [(not (equal? expr1 expr2))
       (values
         (make-color expr1)
         (make-color expr2))]
      [else (values expr1 expr2)])))