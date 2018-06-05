#lang racket 

(provide post 
         http
         http/auth)

(require (prefix-in hc: net/http-client)
         (prefix-in n: net/url)
         (prefix-in url: "./url.rkt")
         (prefix-in j: json))

(define (http url 
              #:method [method #"GET"] 
              #:headers [headers '()]) 
  (define host (n:url-host url))
  (define uri (url:uri url))
  (define-values (res-status res-headers res-in) 
    (hc:http-sendrecv host 
                      uri 
                      #:headers headers
                      #:ssl? #t   
                      #:method method))
 
  (cond
    [(equal? res-status #"HTTP/1.1 400 Bad Request") 
     (displayln (port->string res-in))
     (raise "client.rkt: error in (api-call ...) 400") ]
    [else (j:read-json res-in)]))

(define (post url) (http url #:method #"POST"))

(define (bearer-header auth)
  `(,(format "Authorization: Bearer ~a" auth)))

(define (http/auth auth url #:method method)
  (http url 
        #:method method
        #:headers (bearer-header auth)))

(module+ test
  (require rackunit)

  (test-case
    "GET"
    (define url (url:make "twitter.com" "privacy"))
    (displayln (http url #:method #"GET"))))

