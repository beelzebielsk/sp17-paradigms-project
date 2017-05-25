; Note: These recursive functions are made to run in Scheme and TLS
; Scheme alike, so they're recursive functions
(define call/cc call-with-current-continuation)

; Explanation of how this works:
; The call/cc call always returns one of:
; - A list
; - An atom, which is the message 'was-last.

; - If the call/cc call returns a list, then it is the rest of the list
;   with the last occurrence of e removed in that list, as the last
;   occurrence of e in lst is the last occurrence of e in (cdr lst) (if
;   (cdr lst) has any occurrences at all).
; - If the call/cc call returns the message 'was-last, then the call
;   from the last time the element was found understands that it is the
;   last time and behaves accordingly. All other occurrences know that
;   they are not the last, because what comes back is a list, and they
;   add the current element of lst onto the front of that list.
; Remove left most
((lambda (function lst e) ; Combinator Wrapper
   (function function lst e #f))
 (lambda (F lst e cont)
   (cond ((null? lst) 
          (cond ((eq? cont #f) (quote ()))
                (else (cont 'was-last))))
         ((eq? (car lst) e) 
          (let
            ((retVal (call/cc (lambda (stop)
                                (F F (cdr lst) e stop)))))
            (cond ((eq? retVal 'was-last)
                   (cdr lst))
                  (else
                    (cons (car lst) retVal)))))
         (else (cons (car lst) (F F (cdr lst) e cont)))))
 '(1 5 2 5 3 5 4 5)
 5)

; member?

((lambda (function lst e)
   (function function lst e))
 (lambda (F lst e)
   (cond ((null? lst) #f)
         ((eq? (car lst) e) #t)
         (else (F F (cdr lst) e))))
 '(1 2 3 4)
 5)

; rember

((lambda (function lst e)
   (function function lst e))
 (lambda (F lst e)
   (cond ((null? lst) '())
         ((eq? (car lst) e) (cdr lst))
         (else (cons (car lst) (F F (cdr lst) e)))))
 '(1 2 3 4)
 5)
