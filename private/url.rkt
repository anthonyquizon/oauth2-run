#lang racket 

(provide make
         append-query
         get-query
         uri)

(require (prefix-in n: net/url)
         threading)

(define (string->path_list path)
  (define (f x) (n:make-path/param x '()))
  (map f (string-split path "/")))

(define (make host 
              path 
              #:params [params '()]
              #:port [port 3000]  
              #:scheme [scheme "https"])
  (define path^ (string->path_list path))
  (n:url scheme #f host port #t path^ params #f))

(define (uri url)
  (define head 
    (string-join 
      `(,(n:url-scheme url) "://" ,(n:url-host url) ":" ,(~a (n:url-port url))) ""))

  (~> (n:url->string url)
      (string-replace _ head "")))

(module+ test 
  (require rackunit)

  (test-case
    "uri"
    (define a (make "foo" "/bar/baz"))
    (check-equal? 
      (uri a)
      "/bar/baz"
      ))) 

(define (append-query url query) 
  (define url^ (n:string->url url))
  (define query^ (append (n:url-query url^) query))

  (struct-copy n:url url^ [query query^]))

(define (get-query url)
  (define url^  
    (cond 
      [(string? url) (n:string->url url)]
      [else url]))

  (define query (n:url-query url^))

  (make-hash query))
