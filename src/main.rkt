; main.rkt
; Copyright 2014 Remy E. Goldschmidt <taktoa@gmail.com>
; This file is part of RacketTSBannerGen.
;    RacketTSBannerGen is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    RacketTSBannerGen is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with RacketTSBannerGen. If not, see <http://www.gnu.org/licenses/>.

#lang racket

(require xml
         racket/date
         racket/draw
         web-server/servlet-env
         web-server/http/response-structs)

(define template-path "../res/template.tmp")

(permissive-xexprs #t)

(define template-xml
  (call-with-input-file template-path
    (λ (in) (xml->xexpr (read-xml/element in)))))

(define (explore-xexpr xml xfrm rdc)
  (match xml
    [(? string? str)        (xfrm str)]
    [(? symbol? sym)        (xfrm sym)]
    [(? valid-char? chr)    (xfrm chr)]
    [(? comment? com)       com]
    [(? p-i? prc)           prc]
    [(? cdata? cdt)         cdt]
    [(list-rest sym attr x) (rdc (append (list sym (map xfrm attr)) 
                                         (map (λ (a) (explore-xexpr a xfrm rdc)) x)))]
    [(cons sym x)           (rdc (cons sym (xfrm x)))]))

(define (id-transform i) i)

;play "\u25B6"
;pause "\u2759\u2759"
;stop "\u25FE"

(define (mocp-query-str fmt) (string-append "mocp -Q " (string #\") fmt (string #\")))

(define (mocp-query-proc fmt) (process (mocp-query-str fmt)))

(define (mocp-query fmt)
  (match (mocp-query-proc fmt)
    [(list so _ _ _ _) (read-line so)]
    [_ (error "you fucked up in a different way")]))

;(map mocp-query (list "%state" "%file" "%title" "%artist" "%song" "%album" "%tt" "%tl" "%ts" "%ct" "%cs" "%b" "%r"))

(define (proc-time str)
  (match (string-split str)
    [(list scs sts) (format "~a%" (quotient (* 100 (string->number scs)) (string->number sts)))]
    [_ (error "fuuuck")]))

(define (replace-hash)
  (hash 'svg-image    "../res/base.png"
        'svg-font     "serif"
        'svg-style    "normal"
        'svg-time     (date->string (current-date))
        'song-file    (mocp-query "%file")
        'song-title   (mocp-query "%song")
        'song-artist  (mocp-query "%artist")
        'song-album   (mocp-query "%album")
        'song-time    (proc-time (mocp-query "%cs %ts"))
        'song-state   "p"))
;"\u25B6"))

(define replacer (replace-hash))

(define (subst-transform i)
  (match i
    [(list a x) (list a (hash-ref replacer (string->symbol x) x))]
    [x          (hash-ref replacer (string->symbol x) x)]))

(define (trim-tmp-rdc i)
  (match i
    [(list 'tmp (list-no-order (cons 'id x) ...) ...) (caaar x)]
    [a a]))

(define id-explored (explore-xexpr template-xml id-transform id-transform))
(unless (equal? id-explored template-xml) (error "you fucked up somehow"))
(define (generate-svg)
  (call-with-output-string
   (λ (in) (write-xexpr (explore-xexpr template-xml subst-transform trim-tmp-rdc) in))))

(define (runshit cmd str)
  (match (process cmd)
    [(list sout sin _ _ ctrl)
     (write-string str sin)
     (ctrl 'wait)
     (define result (port->bytes sout))
     (close-input-port sout)
     (close-output-port sin)
     result]
    [_ (error "whut")]))
 
(define (read-ppm port)
  (parameterize ([current-input-port port])
    (define magic (read-line))
    (match-define (list w h) (string-split (read-line) " "))
    (define width (string->number w))
    (define height (string->number h))
    (define maxcol (string->number (read-line)))
    (define bm (make-object bitmap% width height))
    (define dc (new bitmap-dc% [bitmap bm]))
    (send dc set-smoothing 'unsmoothed)
    (define (adjust v) (* 255 (/ v maxcol)))
    (for/list ([x width])
      (for/list ([y height])
        (define red (read-byte))
        (define green (read-byte))
        (define blue (read-byte))
        (define color (make-object color% (adjust red) (adjust green) (adjust blue)))
        (send dc set-pen color 1 'solid)
        (send dc draw-point x y)))
    bm))
 
(define (svg->png filename)
  (define command (format "convert svg:~a png:-" filename))
  (match-define (list in out pid err ctrl)  (process command))
  (define bmp (port->bytes in))
  (close-input-port in)
  (close-output-port out)
  bmp)

(define tmp "tmp.svg")
(call-with-output-file tmp
    (lambda (out) (display (generate-svg) out))
  #:exists 'replace)

(define (start req)
  (response/output (svg->png tmp) #:mime-type #"image/png"))

(serve/servlet start)



;
;(define (im-convert in-str)
;  (with-output-to-bytes
;   (λ () (with-input-from-string
;    in-str
;    (λ () (system "convert svg:- -colors 16 png:-"))))))
;
;
;(let ([out (open-output-bytes)])
;  (process/ports out (open-input-string (generate-svg)) 'stdout "convert svg:- -colors 16 png:-")
;  (get-output-bytes out))
;
;(im-convert (generate-svg))

;(call-with-input-file (string-append (path->string pth) "") (λ (in) (port->string in)) #:mode 'text)

;(call-with-atomic-output-file
; (make-temporary-file)
; (λ (out pth)
;   (generate-svg out)
;   (im-convert "svg" pth "png")
;   ))

;(provide
; (all-defined-out))
;(define music-server<%>
;  (interface ()
;    get-state
;    update))
;
;(define mocp-server%
;  (class object% (music-server<%>)
;    
;    (super-new)))
;
;(define (show-differences expr1 expr2)
;  (let-values ([(s1 s2)
;                (get-differences expr1 expr2)])
;    (pretty-print s1)
;    (pretty-print s2)))
;
;(define-syntax (show-differences-syntax stx)
;  (syntax-case stx ()
;    [(_ expr1 expr2)
;     (begin
;       (let-values ([(e1 e2)
;                     (get-differences (syntax->datum #'expr1)
;                                      (syntax->datum #'expr2))])
;         (with-syntax ([(s1 s2) (list e1 e2)])
;           #'(begin
;               (pretty-print 's1)
;               (pretty-print 's2)))))]))
;
;(show-differences explored template-xml)