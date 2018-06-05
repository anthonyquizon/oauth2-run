#lang racket 


(provide authenticate)

(require (prefix-in ws: web-server/servlet)
         (prefix-in we: web-server/servlet-env)
         (prefix-in wd: web-server/dispatch)
         (prefix-in wh: web-server/http)
         (prefix-in url: "./url.rkt")
         (prefix-in c: "./client.rkt")
         json)

(define (handle-init id auth-url redirect-url) 
  (lambda (_req)
    (define query `((client_id . ,id)
                    (redirect_uri . ,redirect-url)))
    (define url (url:append-query auth-url query))
    (wh:redirect-to url)))

(define (handle-auth #:channel ch 
                     #:id id 
                     #:secret secret 
                     #:token-url token-url 
                     #:redirect-url redirect-url
                     #:exec-post exec-post)
  (lambda (req)
    (define req-url (wh:request-uri req))
    (define code (hash-ref (url:get-query req-url) 'code))
    (define query `((client_id . ,id)
                    (redirect_uri . ,redirect-url)
                    (client_secret . ,secret)
                    (code . ,code)))
    (define url (url:append-query token-url query))
    (define auth-token (exec-post url))

    ;;TODO parse json

    (channel-put ch auth-token)))

(define ((authenticate #:token-url token-url 
                       #:auth-url auth-url 
                       #:id id
                       #:secret secret 
                       #:redirect-url redirect-url))

  (define ch (make-channel))

  (define-values (dispatch url)
    (wd:dispatch-rules
      [("auth" "response") 
       (handle-auth #:channel ch 
                    #:id id 
                    #:secret secret 
                    #:token-url token-url 
                    #:redirect-url redirect-url
                    #:exec-post c:post)]
      [else (handle-init id auth-url redirect-url)]))

  ;;TODO kill server after get
  (we:serve/servlet dispatch
                    #:servlet-regexp #rx""
                    #:port 8080)  

  (channel-get ch))

(module+ test
  (require rackunit
           net/url)

  (test-case 
    "handle-auth"
    (define ch (make-channel))
    (define id "1234")
    (define secret "secret")
    (define token-url "http://token-url/foo/bar")
    (define redirect-url "http://redirect-url/baz/quux")
    (define req (wh:make-request #"GET" (string->url "http://foo.com?code=0001") '() (delay '()) #f "" 123 ""))
    (define exec-post (lambda (foo) 'hello))

    (thread 
      (lambda () 
        ((handle-auth #:channel ch 
                      #:id id 
                      #:secret secret
                      #:token-url token-url
                      #:redirect-url redirect-url
                      #:exec-post exec-post) req)))

    (check-equal? 
      (channel-get ch)
      'hello)))
