#lang racket

(require (prefix-in oauth2: "../main.rkt")
         racket/runtime-path)

(define-runtime-path cwd "./")

(define ((update-list cons-url auth) id) 
  (define url (cons-url id))
  (displayln url)
  (displayln (oauth2:call auth url))
  
  ;;get parent
  ;;update title

  )

(define (run config auth)
  (define (cons-url id) 
    (format 
      "https://3.basecampapi.com/~a/buckets/~a/todolists/~a/todos.json"
      (hash-ref config "account_id")
      (hash-ref config "bucket")
      id)) 


  (for-each (update-list cons-url auth)
    (hash-ref config "todolists"))
  
  )

(module+ main
 (oauth2:run cwd run))


