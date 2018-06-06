#lang racket 

(provide post 
         http
         http/auth)

(require (prefix-in hc: net/http-client)
         (prefix-in n: net/url)
         (prefix-in nh: net/head)
         (prefix-in url: "./url.rkt")
         (prefix-in j: json)
         threading)

(define (byte-pair->string-pair x)
  (cons (string->symbol (~a (car x)))
        (~a (cdr x))))

(define (parse-headers headers)
  (define fields 
    (~> (map nh:extract-all-fields headers)
        (apply append _)
        (map byte-pair->string-pair _)))

  (make-hash fields))

;;TODO use persistant connections
(define (http url 
              #:method [method #"GET"] 
              #:data [data #f]
              #:headers [headers '()]) 
  (define host (n:url-host url))
  (define uri (url:uri url))

  (define data^ 
    (if (not data) #f (j:jsexpr->string data)))

  (define headers^
    (if (not data) headers
      (cons "Content-Type: application/json" headers)))

  (define-values (res-status res-headers res-in) 
    (hc:http-sendrecv host 
                      uri 
                      #:headers headers^
                      #:ssl? #t   
                      #:data data^
                      #:method method))

  (cond
    [(equal? res-status #"HTTP/1.1 400 Bad Request") 
     (displayln (port->string res-in))
     (raise "client.rkt: error in (api-call ...) 400") ]
    [else 
      (hash 
        'headers (parse-headers res-headers)
        'data (j:read-json res-in))]))

(define (post url) (http url #:method #"POST"))

(define (bearer-header auth)
  `(,(format "Authorization: Bearer ~a" auth)))

(define (http/auth auth url #:method method #:data data)
  (http url 
        #:data data
        #:method method
        #:headers (bearer-header auth)))

(module+ test
  (require rackunit)

  (test-case
    "GET"
    (define url (url:make "twitter.com" "privacy"))
    (displayln (http url #:method #"GET"))))

