#lang racket 

(provide post)

(require (prefix-in hc: net/http-client))

(define (post url) 
  (hc:http-sendrecv url #:method #"POST"))

