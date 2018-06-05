#lang racket 

(provide post 
         http)

(require (prefix-in hc: net/http-client)
         (prefix-in n: net/url)
         (prefix-in url: "./url.rkt")
         (prefix-in j: json))

(define (http url #:method [method #"GET"]) 
  (define host (n:url-host url))
  (define uri (url:uri url))
  (define-values (res-status res-headers res-in) 
    (hc:http-sendrecv host 
                      uri 
                      #:ssl? #t   
                      #:method method))
 
  (cond
    [(equal? res-status #"HTTP/1.1 400 Bad Request") 
     (displayln (port->string res-in))
     (raise "client.rkt: error in (api-call ...) 400") ]
    [else (j:read-json res-in)]))

(define (post url) (http url #:method #"POST"))

(module+ test
  (require rackunit)

  (test-case
    "GET"
    (define url (url:make "twitter.com" "privacy"))
    (displayln (http url #:method #"GET"))))

