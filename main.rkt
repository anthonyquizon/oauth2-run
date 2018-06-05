#lang racket

(provide run 
         call)

(require threading
         (prefix-in y: yaml)
         (prefix-in n: net/url) 
         (prefix-in c: with-cache) 
         "./private/authenticate.rkt"
         (prefix-in client: "./private/client.rkt")
         json)

(define (call auth url
              #:method [method 'GET] 
              #:data [data null])
  ;;TODO data
  (client:http/auth auth url #:method method))

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

 (proc auth))

(module+ test
  (require racket/runtime-path
           (prefix-in url: "./private/url.rkt"))

  (define-runtime-path cwd "./")

  (define (main)
    (define path (build-path cwd "examples"))
    (define (proc auth) 
      (define url (url:make "" ""))
      (displayln (call auth url)))

    (run path proc))

  (main))
