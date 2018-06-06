#lang racket

(require (prefix-in oauth2: "../main.rkt")
         threading
         racket/runtime-path)

(define-runtime-path cwd "./")

(define (get-ids xs)
  (define (f x) (hash-ref x 'id))
  (map f xs))

(define (update-todo auth todo) 
  (define url (hash-ref todo 'url))
  (define parent_title (~> (hash-ref todo 'parent)
                           (hash-ref _ 'title)
                           (string-replace _ "..." "")))
  (define title (~> (hash-ref todo 'content)
                    (string-replace _ parent_title "")
                    (string-trim _)))

  (define data 
    (hash 
      'content (format "~a ~a" parent_title title) 
      'description (hash-ref todo 'description)
      'assignee_ids (get-ids (hash-ref todo 'assignees '()))
      'completion_subscriber_ids (get-ids (hash-ref todo 'completion_subscribers '()))
      'notify #f
      'due_on (hash-ref todo 'due_on)
      'starts_on (hash-ref todo 'starts_on)))

  (displayln (hash-ref data 'content))

  (oauth2:call auth url #:method 'PUT #:data data))


(define (update-list auth url)
  (define resp (oauth2:call auth url))
  (define headers (hash-ref resp 'headers))
  (define data (hash-ref resp 'data))
  (define link (hash-ref headers 'Link #f)) 

  (for-each 
    (lambda (todo) 
      (update-todo auth todo)) data)
  
  (when link 
    (define url^ (~> (string-split link ">;")
                     (first _)
                     (string-replace _ "<" "")))
    (update-list auth url^)))

(define (run config auth)
  (define (cons-url tail) 
    (format 
      "https://3.basecampapi.com/~a/buckets/~a/~a"
      (hash-ref config "account_id")
      (hash-ref config "bucket")
      tail)) 


  (for-each 
    (lambda (id) 
      (define url (cons-url (format "todolists/~a/todos.json" id)))
      (update-list auth url))

    (hash-ref config "todolists"))
  
  )

(module+ main
 (oauth2:run cwd run))


