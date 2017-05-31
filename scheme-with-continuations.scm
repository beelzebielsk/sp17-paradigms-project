; I'm starting this interpreter from scratch. Using continuations means
; passing them around, because the continuations hold all information
; about what happens with the current expression being evaluated.
;
; If there is an expression which has subexpressions that need to be
; evaluated, then the current expression is placed within the control
; context of the expression and the subexpressions are evaluated.
;
; To make this good, we should not, in any way, leverage continuations
; in the underlying scheme. That means:
; - Do not use call-with-current-continuation or call/cc from Scheme.
; - Do not use any properly recursive calls from underlying scheme, as
;   the underlying scheme does continuation passing. While the meaning
;   functions are necessarily recursive (because anything that's not a
;   simple expression has subexpressions to evaluate, and the process of
;   evaluating them is the same as the original process), it doesn't
;   have to be *properly recursive*. Instead of keeping the
;   interpreter's control context in the underlying scheme, relying on
;   that to remember how/when to combine values by properly returning
;   values from recursive function calls, we just add things to be done
;   in the control context of the interpreter.

; So, what should change control context? What shouldn't?
; - Things that can be immediately evaluated, such as primitives,
;   probably shouldn't. They should just be returned immediately to the
;   current continuation.
; - Things that have subexpressions probably should, because the meaning
;   will be determined by the subexpressions. We have to build the
;   meaning of the expression in terms of the values that will come out
;   of the subexpressions by building continuations around those
;   subexpressions.
; - Let/lambda expressions, I'm not sure about. The value of the
;   expression is the value of the subexpression, I suppose, but some
;   subexpressions have to be evaluated immediately, like the values of
;   the arguments.


; Constructors/Accessors/Mutators: {{{ ;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;; Constructor/accessors for compound data types.
(define build-compound list)
(define first cadr)
(define second caddr)
(define third cadddr)
; If calls to 'cons' start appearing within the interpreter, I'm
; breaking abstraction barriers.
(define compound? pair?)

;;;;;;;;;; Boolean Values
; Map from Interpreter Booleans to Scheme Booleans
(define bool-val
	(lambda (val)
		(cond ((eq? val 'True) #t)
					((eq? val 'False) #f)
					(else (throw-error)))))

;;;;;;;;;; Errors
; Force a Scheme error. Will eventually be replaced by causing
; Interpreter Errors, which will be handled through continuations.
(define throw-error
	(lambda () (car '())))


; }}} ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Meaning needs an expression, a data context for the expression, and a
; control context for the expression.
; - Environments are data contexts.
; - Continuations are control contexts.
; TODO: Fill this in.
(define meaning
	(lambda (environment statement continuation)))

; I'm pretty sure that expression-to-action and the action functions
; replace the `cases` function, which seems to have the power to both
; syntax check and execute code at the same time. Our program would
; instead look at the syntax and decide what to do, which is sorta
; similar, except that we're not guarding against malformed expressions.

; Come to think of it, because I've got a continuation passing
; interpeter, I can do error checking and reporting pretty easily! I can
; just create error reporting continuations and execute those when we've
; got an error. The meaning of a malformed expression is arbitrarily
; made to be an error message and immediately quitting the program.

(define expression-to-action
	(lambda (expression)
		(if (compound? expression)
			(list-to-action expression)
			(atom-to-action expression))))

; TODO: Fill this in.
(define list-to-action
	(lambda (expression)))

; TODO: Fill this in.
(define atom-to-action
	(lambda (expression)))
