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

(define (call auth url
              #:method [method 'GET] 
              #:data [data null]
              #:cached? [cached? #t])

  (define url^ (url:string->url url))
  (define hash-name (nu:uri-encode (format "~a-~a" url method))) ;;TODO data

  (c:with-cache 
    #:use-cache? cached?
    (c:cachefile hash-name)
    (lambda () 
      (client:http/auth auth url^ #:method method))))

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
