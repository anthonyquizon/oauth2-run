#lang racket

(provide run 
         call)

(require threading
         (prefix-in y: yaml)
         (prefix-in nu: net/uri-codec)
         (prefix-in url: "./private/url.rkt")  
         (prefix-in c: with-cache) 
         "./private/authenticate.rkt"
         (prefix-in client: "./private/client.rkt")
         json)


(define regex #rx"(https://)|(/)|(www)|(\\.)|( )|(\\-)")

(define (call auth url
              #:method [method 'GET] 
              #:data [data #f]
              #:cached? [cached? #t])

  (define url^ (url:string->url url))
  (define (f) 
    (client:http/auth auth 
                      url^ 
                      #:method method
                      #:data data))

  (cond
    [(equal? method 'GET) 
     (define hash-name (~> (~a url)
                           (regexp-replace* regex _ "")
                           nu:uri-encode)) 

     (c:with-cache (c:cachefile hash-name) f)]
    [else (f)]))

(define cache-file "oauth.cache")

(define (run path proc)
  (define config-path (build-path path "config.yml"))
  (define config (~> config-path open-input-file y:read-yaml))
  (define auth 
    (c:with-cache (c:cachefile cache-file) 
                  (authenticate #:token-url (hash-ref config "token_url")
                                #:auth-url (hash-ref config "auth_url")
                                #:id (hash-ref config "id")
                                #:secret (hash-ref config "secret")
                                #:redirect-url (hash-ref config "redirect_url")))) 

 (proc config auth))

(module+ test
  (require racket/runtime-path
           (prefix-in url: "./private/url.rkt"))

  (define-runtime-path cwd "./")

  (define (main)
    (define path (build-path cwd "examples"))
    (define (proc config auth) 
      (displayln auth))

    (run path proc))

  (main))
