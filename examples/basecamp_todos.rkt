#lang racket

(require (prefix-in oauth2: "../main.rkt")
         threading
         racket/runtime-path)

(define-runtime-path cwd "./")

(define ((update-list cons-url auth) id) 
  (define url (cons-url id))
  (define data (oauth2:call auth url))

  (define (f todo) 
    (define parent_title (~> (hash-ref todo 'parent)
                             (hash-ref _ 'title)))
    (define title (hash-ref todo 'title))
    (define title^ (format "~a ~a" parent_title title))
    (displayln title^)
    )
  
  (for-each f data))

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


