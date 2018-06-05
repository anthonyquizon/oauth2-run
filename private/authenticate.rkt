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
                    (redirect_url . ,redirect-url)))
    (define url (url:append-query auth-url query))
    (wh:redirect-to url)))

(define (handle-auth ch id secret token-url redirect-url)
  (lambda (req)
    (define req-url (wh:request-uri req))
    (define code (hash-ref (url:get-query url) "code"))
    (define query `((client_id . ,id)
                    (redirect_url . ,redirect-url)
                    (client_secret . ,secret)
                    (code . ,code)))
    (define url (url:append-query token-url query))

    (displayln (c:post url))))

(define ((authenticate ch
                       #:token-url token-url 
                       #:auth-url auth-url 
                       #:id id
                       #:secret secret 
                       #:redirect-url redirect-url))

  (define ch (make-channel))

  (define-values (dispatch url)
    (wd:dispatch-rules
      [("auth" "response") (handle-auth ch id secret token-url redirect-url)]
      [else (handle-init id auth-url redirect-url)]))

  ;;TODO kill server after get
  (we:serve/servlet dispatch
                    #:servlet-regexp #rx""
                    #:port 8080)  

  (channel-get ch)
  )
