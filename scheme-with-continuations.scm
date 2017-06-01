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
;   the arguments. Chances are, applications should open up new 

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
(define bool-val?
	(lambda (val)
		(or (eq? val 'True) (eq? val 'False))))

;;;;;;;;;; Errors
; Force a Scheme error. Will eventually be replaced by causing
; Interpreter Errors, which will be handled through continuations.
(define throw-error
	(lambda () 
		(display "Interpreter error, or area was off-limits.")
		(newline)
		(car '())))

;;;;;;;;;; Special Continuations
(define end-cont 
	(lambda (value)
		(display "The value of the expression was: ")
		(display value) (newline)
		(display '(End program)) (newline)))

;;;;;;;;;; Environments
; The environment is a list of ribs.
(define empty-environment '())
(define empty-environment? (lambda (env) (eq? env '())))
(define top-rib car)
(define pop-rib cdr)
(define rib-names car)
(define rib-values cadr)
; Add a new rib to the environment.
(define extend-environment
	(lambda (names vals environment)
		(cons (list names vals) environment)))
; Look up a name in the top rib, or in the rest of the ribs.
(define lookup-in-environment
	(lambda (name environment)
		(cond ((empty-environment? environment) (throw-error))
					(else (lookup-in-rib
									name
									(rib-names (top-rib environment))
									(rib-values (top-rib environment))
									(lambda () (lookup-in-environment
															 name
															 (pop-rib environment))))))))

; Look up a name in a rib, then find the corresponding value.
(define lookup-in-rib
	(lambda (name name-list value-list failure-callback)
		(cond ((null? name-list) (failure-callback))
					((eq? (car name-list) name) (car value-list))
					(else (lookup-in-rib
									name
									(cdr name-list)
									(cdr value-list)
									failure-callback)))))


; }}} ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define value
	(lambda (expression)
		(meaning empty-environment expression end-cont)))

; Meaning needs an expression, a data context for the expression, and a
; control context for the expression.
; - Environments are data contexts.
; - Continuations are control contexts.
; TODO: Fill this in.
(define meaning
	(lambda (environment expression continuation)
		((expression-to-action expression)
		 environment 
		 expression 
		 continuation)))

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
	(lambda (expression)
		(let ((first-word (first expression)))
			(cond ((eq? first-word (quote cond)) *cond)
						((eq? first-word (quote lambda)) *lambda)
						((eq? first-word (quote let)) *let)
						((eq? first-word (quote if)) *if)
						(else *application)))))

; TODO: Fill this in.
(define atom-to-action
	(lambda (expression)
		(cond ((number? expression) *const)
					((bool-val? expression) *const)
					(else (throw-error)))))

; Action Functions: {{{ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Gives the meaning of language primitives:
; - numbers
; - booleans
(define *const
	(lambda (environment expression continuation)
		(cond ((number? expression) (continuation expression))
					((bool-val? expression) (continuation (bool-val expression)))
					(else (throw-error)))))

; Create a continuation 
(define *cond
	(lambda (environment expression continuation)
		(throw-error)))

; Create a continuation 
(define *if
	(lambda (environment expression continuation)
		(throw-error)))

; Create a continuation 
(define *let
	(lambda (environment expression continuation)
		(throw-error)))

; Create a continuation 
(define *lambda
	(lambda (environment expression continuation)
		(throw-error)))

; Create a continuation 
(define *application
	(lambda (environment expression continuation)
		(throw-error)))

; }}} ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
