; Note: These recursive functions are made to run in Scheme and TLS
; Scheme alike, so they're recursive functions
(define call/cc call-with-current-continuation)
; (define add +) ; To mirror TLS Scheme.

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

; Would continuations preserve a call stack? Are they in the
; continuation? Or does the stack, in the background, work with the
; continuations in such a way that recalling a continuation can
; "resurrect" a stack of deferred calls.

; Answer: They absolutely can.
; The idea of the bottom function is to stop execution right before any
; of the functions return. The returned value is the continuation to
; that whole chain of deferred calls, each printing numbers in reverse
; order. 
; When the continuation is called, the entire chain of calls happens.
; Continuations might, with a terrible implementation, hold a copy of
; the stack. Though, this shouldn't be quite right, because
; continuations aren't process snapshots.

(define (bs-make-sequence-print x)
	(define (helper x exit-cont)
		(if (< x 10)
			(helper (+ x 1) exit-cont)
			(call/cc (lambda (come-back-here) (exit-cont come-back-here))))
		(print-line x))
	(call/cc (lambda (exit-cont) (helper 0 exit-cont))))

(define sequen (bs-make-sequence-print 0))
(print-line 'sequence:)
(sequen 0)

; Rewriting the above test for use in TLS Scheme.

((lambda (x) ; The make-print-sequence wrapper
	 ((lambda (y) ; The thing to do with the value of the print-continuation.
			; Verifies that the continuation is actually saved by doing
			; something before the continuation gets used.
			(begin  
				(print-line 'about-to-use-continuation)
				(y 0)))
		; This is a combinator. It's output is the output from the recursive
		; function, which is a continuation, and that continuation will
		; become y.
		((lambda (function arg)
			 (call/cc (lambda (cont) (function function arg cont))))
		 ; Recursive function.
		 (lambda (F arg quick-exit)
			 (begin 
				 (cond ((< arg 10) (F F (+ arg 1) quick-exit))
							 (else (call/cc (lambda (come-back) (quick-exit come-back)))))
				 (print-line arg)
				 ; This isn't just some flourish. The way that the function is
				 ; written, once the continuation is called, the function will
				 ; finish up, and when it does, it should return a function to
				 ; bind to the parameter 'y'. If there's no function, there's an
				 ; error.
				 (lambda (x) 'and-we-finish)))
		 x))) ; arg argument to the combinator.
 0) ; x input to the outermost function

;;; Testing defining these as sexps

; Remove left most
(define remove-left-most-test
	'((lambda (function lst e) ; Combinator Wrapper
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
		(1 5 2 5 3 5 4 5)
		5)
	)

; member?

(define member?-test
	'((lambda (function lst e)
			(function function lst e))
		(lambda (F lst e)
			(cond ((null? lst) #f)
						((eq? (car lst) e) #t)
						(else (F F (cdr lst) e))))
		(1 2 3 4)
		5)
	)

; rember

(define rember-test 
	'((lambda (function lst e)
			(function function lst e))
		(lambda (F lst e)
			(cond ((null? lst) '())
						((eq? (car lst) e) (cdr lst))
						(else (cons (car lst) (F F (cdr lst) e)))))
		'(1 2 3 4)
		5)
	)

; Would continuations preserve a call stack? Are they in the
; continuation? Or does the stack, in the background, work with the
; continuations in such a way that recalling a continuation can
; "resurrect" a stack of deferred calls.

; Answer: They absolutely can.
; The idea of the bottom function is to stop execution right before any
; of the functions return. The returned value is the continuation to
; that whole chain of deferred calls, each printing numbers in reverse
; order. 
; When the continuation is called, the entire chain of calls happens.
; Continuations might, with a terrible implementation, hold a copy of
; the stack. Though, this shouldn't be quite right, because
; continuations aren't process snapshots.

(define (bs-make-sequence-print x)
	(define (helper x exit-cont)
		(if (< x 10)
			(helper (+ x 1) exit-cont)
			(call/cc (lambda (come-back-here) (exit-cont come-back-here))))
		(print-line x))
	(call/cc (lambda (exit-cont) (helper 0 exit-cont))))

;(define sequen (bs-make-sequence-print 0))
;(print-line 'sequence:)
;(sequen 0)

; Rewriting the above test for use in TLS Scheme.

(define make-sequence-test 
	'((lambda (x) ; The make-print-sequence wrapper
			((lambda (y) ; The thing to do with the value of the print-continuation.
				 ; Verifies that the continuation is actually saved by doing
				 ; something before the continuation gets used.
				 (begin  
					 (print-line 'about-to-use-continuation)
					 (y 0)))
			 ; This is a combinator. It's output is the output from the recursive
			 ; function, which is a continuation, and that continuation will
			 ; become y.
			 ((lambda (function arg)
					(call/cc (lambda (cont) (function function arg cont))))
				; Recursive function.
				(lambda (F arg quick-exit)
					(begin 
						(cond ((< arg 10) (F F (+ arg 1) quick-exit))
									(else (call/cc (lambda (come-back) (quick-exit come-back)))))
						(print-line arg)
						; This isn't just some flourish. The way that the function is
						; written, once the continuation is called, the function will
						; finish up, and when it does, it should return a function to
						; bind to the parameter 'y'. If there's no function, there's an
						; error.
						(lambda (x) 'and-we-finish)))
				x))) ; arg argument to the combinator.
		0) ; x input to the outermost function
	)

