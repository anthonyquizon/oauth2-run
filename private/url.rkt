#lang racket 

(provide make
         append-query
         get-query)

(require (prefix-in n: net/url))

(define (string->path_list path)
  (define (f x) (n:make-path/param x '()))
  (map f (string-split path "/")))

(define (make host 
              path 
              params
              #:port [port 3000]  
              #:scheme [scheme "https"])
  (define path^ (string->path_list path))
  (n:url->string 
    (n:url scheme #f host port #t path^ params #f)))

(define (append-query url query) 
  (define url^ (n:string->url url))
  (define query^ (append (n:url-query url^) query))

  (n:url->string 
    (struct-copy n:url url^ [query query^])))

(define (get-query url)
  (define url^  
    (cond 
      [(string? url) (n:string->url url)]
      [else url]))

  (define query (n:url-query url^))

  (make-hash query))
