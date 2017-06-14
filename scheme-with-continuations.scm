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
; - Do not use any properly recursive calls from underlying scheme, that
;   will recurse *with unbounded depth*. The underlying scheme does
;   continuation passing, so such calls should be avoided where
;   possible. However, such calls may be necessary, and they aren't
;   really cheating so long as the context of the interpreter
;   instructions is not stored within the underlying scheme. The only
;   context that should be stored in the underlying scheme is control
;   context that's generated for getting the correct action functions
;   and such. Such calls will always be of bounded recursion depth.
;   Instead of keeping the interpreter's control context in the
;   underlying scheme, relying on that to remember how/when to combine
;   values by properly returning values from recursive function calls,
;   we just add things to be done in the control context of the
;   meaning function.

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

(define print-line
	(lambda (thing) (display thing) (newline)))

; Constructors/Accessors/Mutators: {{{ ;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;; Constructor/accessors for compound data types.
(define build-compound list)
(define first car)
(define second cadr)
(define third caddr)
; If calls to 'cons' start appearing within the interpreter, I'm
; breaking abstraction barriers. So I'm defining an abstraction to a
; 'compound' datatype, which just expresses that the datatype bundles
; different values together. It's basically just list. The only
; difference is that there's flexibility in the underlying
; implementation now.
(define compound? pair?)

;;;;;;;;;; Boolean Values
; Map from Interpreter Booleans to Scheme Booleans
(define bool-val
	(lambda (val)
		(cond ((eq? val 'True) #t)
					((eq? val 'False) #f)
					(else (throw-error)))))
(define scheme-bool-to-interpreter-bool
	(lambda (val)
		(if val 'True 'False)))

(define bool-val?
	(lambda (val)
		(or (eq? val 'True) (eq? val 'False))))

;;;;;;;;;; Lambdas
; A valid lambda expression is of the form:
; (lambda (arg-list) body)
(define lambda-exp-formals
	(lambda (expression) (second expression)))
(define lambda-exp-body 
	(lambda (expression) (third expression)))
(define build-lambda
	(lambda (environment lambda-exp)
		(build-compound 
			'non-primitive
			(build-compound environment
											(lambda-exp-formals lambda-exp)
											(lambda-exp-body lambda-exp)))))
(define lambda-env
	(lambda (lambda-val)
		(first (second lambda-val))))
(define lambda-formals
	(lambda (lambda-val)
		(second (second lambda-val))))
(define lambda-body
	(lambda (lambda-val)
		(third (second lambda-val))))
(define build-primitive
	(lambda (expression)
		(build-compound 'primitive expression)))

; Leaves only the function, with no tag.
(define get-function-value
	(lambda (func-val)
		(second func-val)))
(define primitive?
	(lambda (func-value)
		(eq? 'primitive (first func-value))))
(define non-primitive?
	(lambda (func-value)
		(eq? 'non-primitive (first func-value))))

;;;;;;;;;; Errors
; Force a Scheme error. Will eventually be replaced by causing
; Interpreter Errors, which will be handled through continuations.
; The 'area was off-limits' is currently temporary. This error is being
; used in places where functions are defined, but not implemented.
(define throw-error
	(lambda () 
		(display "Interpreter error, or area was off-limits.")
		(newline)
		(car '())))

(define make-error
	(lambda (msg)
		(lambda ()
			(print-line msg)
			(car '()))))

(define error-value-not-found
	(make-error "Value not found."))

(define no-function-of-type
	(make-error "Function is neither primitive nor non-primitive."))

;;;;;;;;;; Special Continuations
; This continuation is the final thing to do. It is the absolute
; smallest control context of any instruction, and any other control
; context builds upon this. It is pretty much the 'empty continuation'.
(define end-cont 
	(lambda (value)
		(display "The value of the expression was: ")
		(print-line value)
		(print-line '(End program))))

;;;;;;;;;; Environments
; The environment is a list of ribs.
(define empty-environment '())
(define empty-environment? (lambda (env) (eq? env empty-environment)))
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
		(cond ((empty-environment? environment) 
					 (display "The name being looked up: ")
					 (print-line name)
					 (display "The environment at the time of lookup: ")
					 (print-line environment)
					 (error-value-not-found))
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
			(cond ((eq? first-word (quote cond))   *cond)
						((eq? first-word (quote if))     *if)
						((eq? first-word (quote let))    *let)
						((eq? first-word (quote lambda)) *lambda)
						((eq? first-word (quote quote))  *quote)
						(else *application)))))

; These are the primitives of the language:
; The two datatypes which are atoms are numbers and booleans.
; Every other atom is some sort of name. The name either refers to a
; primitive function, in which case it will be evaluated by *const, or
; to an identifier in an environment, in which case it will be evaluated
; by *identifier.
(define atom-to-action
	(lambda (expression)
		(cond ((number? expression) *const)
					((bool-val? expression) *const)
					((eq? '+ expression) *const)
					((eq? '* expression) *const)
					((eq? '- expression) *const)
					((eq? 'expt expression) *const)
					((eq? 'add1 expression) *const)
					((eq? 'sub1 expression) *const)
					((eq? 'cons expression) *const)
					((eq? 'car expression) *const)
					((eq? 'cdr expression) *const)
					((eq? 'list expression) *const)
					((eq? 'and expression) *const)
					((eq? 'or expression) *const)
					((eq? 'not expression) *const)
					((eq? 'number? expression) *const)
					((eq? 'null? expression) *const)
					((eq? 'pair? expression) *const)
					((eq? 'atom? expression) *const)
					((eq? 'zero? expression) *const)
					((eq? 'positive? expression) *const)
					((eq? 'negative? expression) *const)
					;((eq? 'call/cc expression) *const) ; Not yet!
					(else *identifier))))

; Action Functions: {{{ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Gives the meaning of language primitives:
; These should not create continuations. Primitives do not require
; extension of control context to evaluate. We either lookup a name, or
; evaluate a number, or evaluate a boolean, or evaluate a primitive
; function.
; - numbers
; - booleans
; - primitives
(define *const
	(lambda (environment expression continuation)
		(cond ((number? expression) (continuation expression))
					((bool-val? expression) (continuation (bool-val expression)))
					(else (continuation (build-compound 'primitive expression))))))

; Searches for the name, and fails if it doesn't find it.
; TODO: Rework this to work with actual error throwing/catching
; mechanisms. Consider building several continuations and using the
; appropriate one: an error continuation, a normal (try) continuation.
; So on and so forth.
(define *identifier
	(lambda (environment expression continuation)
		(continuation (lookup-in-environment expression environment))))

; The synax of a quote expression should be:
;		(quote exp)
(define *quote
	(lambda (environment expression continuation)
		(continuation expression)))


; This should actually extend the control context, because we should
; avoid extending control context in underylying scheme as much as
; possible. The cond consists of, at minimum, a quesiton and answer. The
; questions should probably extend the control context, and their value
; should be a boolean value. The value of the overall cond-- if any, is
; the value of the answer, or the value of the rest of the cond.
; Another argument as to why it should open up new contexts: if I 
; create a continuation during a question and return back to it, then
; wouldn't I expect to go through the rest of the cond expression? I
; should create these functions with the assumption that *any*
; sub-expression could be call/cc. That's another way to guide my
; creation of actions.
; Answers should not extend the control context, because the value of
; the answer is the value of the cond. Once we come back with the value,
; that value will just be the value of the cond.
(define *cond
	(lambda (environment expression continuation)
		; Remove the 'cond'. Now we just have a list of questions and
		; answer pairs.
		(evcon environment (cdr expression) continuation)))

; This function evaluates the questions/answers of a cond statement.
; The question is either 'else, or it is an expression. If it is
; 'else, then the value of the cond statement is the value of the
; answer. Therefore, evaluate the answer with the continuation given
; to the cond.
; If the question is not 'else, then we are not sure what to do.
; However, if the question would evaluate to true, then we know to
; return the meaning of the answer of this question/answer pair. And if
; the question would not evaluate to true, then we know to evaluate the
; rest of the cond. Therefore, we create a new continuation, which
; will eventually receive a value. If this value is true, then we
; will return the answer, and if the value is not true, then we will
; return the value of the rest of the cond statement.
(define evcon
	(lambda (environment qa-list continuation)
		(let ((question (caar qa-list)) (answer (cadar qa-list)))
			(cond ((eq? 'else question)
						 ;(print-line "Question was 'else.") ;DEBUG
						 (meaning environment answer continuation))
						; TODO: Am I duplicating the environment more than necessary
						; here?
						(else
							(meaning environment 
											 question 
											 (cond-cont environment 
																	answer 
																	(cdr qa-list) 
																	continuation)))))))

; Returns a new continuation for cond expressions.
; Required when the question is not 'else. Any other question is
; technically an operand to the cond, and must be evaluated before
; continuing to evaluate the cond. Thus, a continuation must be opened
; which will accept the eventual result of evaluating the question, and
; then "do the right thing" with that value.
(define cond-cont
	(lambda (environment answer rest-of-cond old-cont)
		(lambda (value)
			(if value
				(meaning environment answer old-cont)
				(evcon environment rest-of-cond old-cont)))))

(define if-question
	(lambda (expr) (cadr expression)))
(define if-val-when-true
	(lambda (expr) (caddr expression)))
(define if-val-when-false
	(lambda (expr) (cadddr expression)))
; I'm going to build this through an equivalent cond expression.
; The syntax should be:
;		(if question if-true if-false).
; Thus, this is equivalent to:
;		(cond ((question if-true) (else if-false)))
(define *if
	(lambda (environment expression continuation)
		(meaning 
			environment 
			(list 
				'cond 
				(if-question expression)
				(if-val-when-true expression)
				(if-val-when-false expression))
			continuation)))

; TODO: Create a continuation 
; A let with a non-empty a-list has to 'collect a binding', then
; evaluate the let with the rest of the a-list. A let with an empty
; a-list is just the value of it's body, in the environment extended by
; all bindings that it has collected thus far. 
(define *let
	(lambda (environment expression continuation)
		(print-line "Went to *let.")
		(throw-error)))

(define *lambda
	(lambda (environment expression continuation)
		(continuation
			(build-lambda environment expression))))

; TODO: Create a continuation 
; - Do I have to open up a new continuation with each argument that's
;   evaluated, currying the function to apply to the arguments, one at a
;   time? Is there a better and not-cheaty way?
; - The *application just kicks off the process. It should evaluate the
;   function in an extended continuation: one that will know what to do
;   with a fully evaluated function. The information that's needed to
;   evaluate a fully evaluated function is the list of actual
;   parameters. So, this new continuation should evaluate the function
;   as a primitive, if the value that it gets is a primitive function,
;   and it should evaluate the function as a non-primiitve if the value
;   that it gets is a non-primitive function.
(define *application
	(lambda (environment expression continuation)
		(print-line "Went to application.") ;DEBUG
		(meaning environment 
						 (car expression) ; The function of the application.
						 (eval-op-cont environment 
													 (cdr expression) 
													 (list) 
													 continuation))))

; Handles the creation of continuations for evaluating application
; operands. These should work the same for primitive and non-primitive
; functions, and it will be designed as so. The actual application of
; a function on the arguments (and, therefore, deciding which type of
; application function to use) will happen after all of the operands
; have been evaluated.
; This will have to know the remaining parameters to evaluate, and the
; meanings of all the parameters evaluated so far in order to continue
; toward the application of a function.
; The order of items in the list will be the same order as how things
; appeared in the application: so function first, then arg0, arg1, ...
(define get-func-from-params-evaled
	(lambda (params-evaled) (car params-evaled)))
(define get-params-from-params-evaled
	(lambda (params-evaled) (cdr params-evaled)))
(define eval-op-cont
	(lambda (environment remaining-params params-evaled old-cont)
		(lambda (value)
			; If there are no remaining parameters to evaluate, then it's time
			; to move to an application. We have 
			(cond ((null? remaining-params)
							; NOTE: function here is a function value from the
							; interpreted scheme, so it is a compound of the
							; form ('primitive <atom>).
							(let ((params-evaled (append params-evaled (list value))))
								(let ((function (get-func-from-params-evaled params-evaled)))
											(params (get-params-from-params-evaled params-evaled))
									(apply-func environment function params old-cont))))
						 ; The value given is the value of the previous argument to
						 ; be evaluated. There must be at least one, because there
						 ; has to be at least the function, and I'm not currently
						 ; giving any special treatment to evaluating the function.
						 ; This routine of building new eval-op-cont with the same
						 ; old-cont is a way of performing 'evlis' without building
						 ; up any control context in the underlying scheme.
						 (else (meaning environment 
														(car remaining-params)
														(eval-op-cont environment
																					(cdr remaining-params)
																					(append params-evaled 
																									(list value))
																					old-cont)))))))

; I might need two separate continuation builders, but I didn't. All I
; needed was An argument evaluation continuation.
;		- Strangely enough, thinking of how to do things with 2 levels of
;		   control context caused me to successfully do things with just 1
;		   level of control context. The continuation builder making more of
;		   itself, but not based on itself was everything that I needed.
;		   eval-op-cont would either go the the actual function application
;		   if it had everything it needed (a full list of evaluated
;		   parameters), or it would evaluate the current parameter, and send
;		   that value to a new eval-op-cont whose job is to evaluate all the
;		   remaining parameters (if there are any), and then do the right
;		   thing with those.
; I'm leaving the old idea here for now in this commit, and it will be
; removed later. It might help in the future.
;		- This will open up a new continuation, and within that
;		   continuation, we'll have context extensions for each operand. So
;		   if we're n levels deep at the start of function evaluation, then
;		   we'll go to n+1 where we open up a continuation for the
;		   evaluation of all the pieces of the application, and for each
;		   evaluation, we'll go to n+2. Each return from n+2 yields the
;		   meaning of an argument, and from there, we continue to process
;		   the rest of the arguments.
;		- To make it properly iterative, it's likely that we'll have to keep
;		   bouncing between n and n+2. The n+1 will open up n+2, which will
;		   return an argument, and n+1 will open up a new n+1 with the rest
;		   of the arguments to go and figure out. So, n+1 will ask for the
;		   meaning of the first argument in a list of remaining arguments to
;		   go and figure out, then it will pass that meaning to a new n+1
;		   which is given the current list of evaluated arguments, and the
;		   list of arguments that will still be unevaluated by the time we
;		   get to that continuation. If there are no remaining arguments to
;		   consider, we return the arguments from an apply function. That
;		   function will handle the details of whether or not the procedure
;		   from the interpreter is primitive or not, and will then delegate
;		   a new call to a specific type of application. Any of those
;		   delegation functions should pass in the meaning of the
;		   application to the continuation at level n.

(define apply-func
	(lambda (environment function params old-cont)
		(cond ((primitive? function) 
					 (apply-primitive environment function params old-cont))
					((non-primitive? function) 
					 (apply-nonprimitive environment function params old-cont))
					(else (no-function-of-type)))))

(define apply-primitive
	(lambda (environment function params old-cont)
		(let ((func-atom (get-function-value function)))
		(cond ((eq? func-atom '+) 
					 (old-cont (+ (first params) (second params))))
					((eq? func-atom '*)
					 (old-cont (* (first params) (second params))))
					((eq? func-atom '-)
					 (old-cont (- (first params) (second params))))
					((eq? func-atom 'expt)
					 (old-cont (expt (first params) (second params))))
					((eq? func-atom 'add1)
					 (old-cont (+ (first params) 1)))
					((eq? func-atom 'sub1)
					 (old-cont (- (first params) 1)))
					((eq? func-atom 'cons)
					 (old-cont (cons (first params) (second params))))
					((eq? func-atom 'car)
					 (old-cont (car (first params))))
					((eq? func-atom 'cdr)
					 (old-cont (cdr (first params))))
					((eq? func-atom 'list)
					 (old-cont (list (first params) (second params))))
					((eq? func-atom 'and)
					 (old-cont  (and (first params) (second params))))
					((eq? func-atom 'or)
					 (old-cont  (or (first params) (second params))))
					((eq? func-atom 'not)
					 (old-cont  (not (first params))))
					((eq? func-atom 'number?)
					 (old-cont  (number? (first params))))
					((eq? func-atom 'null?)
					 (old-cont  (null? (first params))))
					((eq? func-atom 'pair?)
					 (old-cont  (pair? (first params) )))
					((eq? func-atom 'atom?)
					 (old-cont  (atom? (first params))))
					((eq? func-atom 'zero?)
					 (old-cont  (zero? (first params))))
					((eq? func-atom 'positive?)
					 (old-cont  (positive? (first params))))
					((eq? func-atom 'negative?)
					 (old-cont  (negative? (first params))))
					(else (throw-error))))))

(define apply-nonprimitive
	(lambda (environment function params old-cont)
		(meaning (extend-environment (lambda-formals function)
																 params
																 (lambda-env function))
						 (lambda-body function)
						 old-cont)))
	
					 


; }}} ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
