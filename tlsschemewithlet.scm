; Project Notes: {{{ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Project Steps:
; - Basic TLS Scheme implementation of continuations.
; - Conversion of TLS Scheme to CPS style.
;   - Relates to implementing continuations, because if we have them,
;      then we should leverage them in creating our functions.
;   - It will also be one further step toward making the mechanics of
;      interpreters with continuations more transparent, as we'll be
;      able to see how continuations are used to replace a return
;      statement.
; - Implementing continuations fully, making them fully transparent.
;   - We sorta cheated with our first way. It was just a stepping stone.
;      Ultimately, we didn't make continuations any more transparent
;      than they were with underlying Scheme. They're still opaque. To
;      really implement them, we have to create a value that holds
;      information about program state: where to jump in the program,
;      handling the jump, and perhaps handling some memory concerns.
;      Continuations should be a TLS scheme value, and not just the
;      underlying scheme continuation.
;   - Figure out what information we'd need to create this value.
;   - Figure out how to represent that information in the underlying
;      scheme "virtual machine".
;   - Figure out how to use/apply those values (ie how would we shift
;      around in a TLS scheme program? What's a jump in TLS Scheme?)

; Basic Continuations: What have we done
; - Adding a continuation type, because primitive is not appropriate for
;   it. The second value of a primitive is an atom, which is the name of
;   some underlying scheme function. The second value that we placed,
;   originally is a continuation, so the cond of apply-primitive falls
;   through to the bottom and nothing happens.
;   - We have to make a third type: continuation. It'll look a lot like
;     a primitive: (list (quote continuation) some-cont)
;   - Evaluating it will be farily similar to a primitive: we pass in
;     the argument to it and call it, much like a primitive function.
; - Adding a 'begin' special form, which will allows us to evaluate
;   expressions one after the other. This may not match the R5RS
;   implementation, but I'm unconcerned with that. For one, I only want
;   it to work to allow more complicated expressions, and the details
;   we'd miss probably have to do with state-changing functions like
;   set!, which we are not implementing. The absolute requirements are:
;   - Statements are evaluated, one after the other, in order from left
;     to right.
;   - The meaning of a begin is the meaning of the last expression in
;      the begin.

; - To add a new primitive function:
;   - Add a new entry in atom-to-action that will lead the primitive to
;      *const.
;   - Alter *const with a new primitive function entry.
; - To add a new action:
;   - Figure out if it's an action on an atom or a list, and update the
;      respective expression-to-action function.
;   - Add the new action function (*action-name).
;   - Add any other necessary actions to assist in getting the meaning
;      of the expression.

; }}} ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helpful Expressions: {{{ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; active debug lines: ^\s*[^;]\s\@<!\&.*;DEBUG\s*$
; inactive debug lines: ^\s*;\s\@<!\&.*;DEBUG\s*$
; }}} ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define call/cc call-with-current-continuation)
(define print-line (lambda (x) (display x) (newline)))

; auxiliary functions

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote ())))))

(define first car)

(define second cadr)

(define third caddr)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; Environment Manipulation Functions: {{{ ;;;;;;;;;;;;;;;;;;

(define lookup-in-table
  (lambda (name table table-f)
    (cond 
      ((null? table) (table-f name))
      (else (lookup-in-entry name
                             (car table)
                             (lambda (name)
                               (lookup-in-table name
                                                (cdr table)
                                                table-f)))))))

(define extend-table cons)



(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (names entry)
                          (values entry)
                          entry-f)))



(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name) (car values))
      (else (lookup-in-entry-help name
                                  (cdr names)
                                  (cdr values)
                                  entry-f)))))




(define new-entry build)

(define names
  (lambda (entry) (car entry)))

(define values
  (lambda (entry) (cadr entry)))

; }}} ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define value
  (lambda (e)
    (meaning e (quote () ))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))


(define expression-to-action
  (lambda (e)
    (cond 
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote car)) *const)
      ((eq? e (quote cdr)) *const)
      ((eq? e (quote null?)) *const)
      ((eq? e (quote eq?)) *const)
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote mul)) *const)
      ((eq? e (quote add)) *const)
      ((eq? e (quote number?)) *const)
      ((eq? e (quote call/cc)) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond 
         ((eq? (car e) (quote quote))
          *quote)
         ((eq? (car e) (quote lambda))
          *lambda)
         ((eq? (car e) (quote cond))
          *cond)
         ((eq? (car e) (quote let))
          *let)
         ((eq? (car e) (quote begin))
          *begin)
         (else *application))) ; For identifiers bound to lambdas
      (else *application)))) ; For lambdas


(define *const
  (lambda (e table)
    (cond 
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e)))))


(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))


(define initial-table
  (lambda (name)
    (car (quote ()))))


(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e)))))

(define *let
  (lambda (e table)
    (let ((associated-lambda-exp 
           (lambda-from-let e)))
      (meaning associated-lambda-exp table))))

; Evaluate each expression, one after the other, the meaning of the
; begin is the meaning of the last expression.
; Expects to have at least one expression.
; Form of a begin is: '(begin e1 e2 ... en)', where all ei are valid
; TLS Scheme expressions.
(define *begin
  (lambda (e table)
    ;(print-line 'begin-expression-to-evaluate:) ;DEBUG
    ;(print-line e) ;DEBUG
    (define (helper e table)
      ;(print-line 'values-before-meaning:) ;DEBUG
      ;(print-line 'expression-to-evaluate:) ;DEBUG
      ;(print-line e) ;DEBUG
      ;(print-line 'the-table:) ;DEBUG
      ;(print-line table) ;DEBUG
      (let ((value (meaning (first e) table)))
        ;(print-line 'values-after-meaning:) ;DEBUG
        ;(print-line 'expression-to-evaluate:) ;DEBUG
        ;(print-line e) ;DEBUG
        ;(print-line 'the-table:) ;DEBUG
        ;(print-line table) ;DEBUG
        (cond ((null? (cdr e)) value)
              (else (helper (cdr e) table)))))
    (helper (cdr e) table))) ; Get rid of 'begin token.

(define table-of first)

(define formals-of second)

(define body-of third)

(define evcon
  (lambda (lines table)
    (cond 
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines))
                table))
      ((meaning (question-of (car lines))
                table)
       (meaning (answer-of (car lines))
                table))
      (else (evcon (cdr lines) table)))))


(define else?
  (lambda (x)
    (cond 
      ((atom? x) (eq? x (quote else)))
      (else #f))))

(define question-of first)

(define answer-of second)

(define *cond 
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

(define evlis
  (lambda (args table)
    (cond 
      ((null? args) (quote ()))
      (else
       (cons (meaning (car args) table)
             (evlis (cdr args) table))))))

(define *application
  (lambda (e table)
    (apply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define function-of car)

(define arguments-of cdr)

(define continuation?
  (lambda (l)
    (eq? (first l) (quote continuation))))

(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))



(define apply
  (lambda (fun vals)
    ;(print-line 'entering-apply:) ;DEBUG
    ;(print-line fun) ;DEBUG
    ;(print-line vals) ;DEBUG
    (cond
      ((primitive? fun)
       (apply-primitive
        (second fun) vals))
      ((non-primitive? fun)
       (apply-closure
        (second fun) vals))
       ; We could get the value here, directly, but the actual way of
       ; using continuations could change later, and it would break the
       ; pattern established with the other ways that applications
       ; happen.
       ; Right now, the value of a continuation is the value of an
       ; underlying scheme continuation, but it could eventually become
       ; a TLS Scheme value, of the form:
       ;  '(continuation (state information))
      ((continuation? fun)
       ;(print-line 'evaluating-a-continuation:) ;DEBUG
       (apply-continuation
        (second fun) vals))
      ;(else (print-line 'fell-through-apply-cond) (cond)) ;DEBUG
      )))


(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name (quote cons))
       (cons (first vals) (second vals)))
      ((eq? name (quote car))
       (car (first vals)))
      ((eq? name (quote cdr))
       (cdr (first vals)))
      ((eq? name (quote null?))
       (null? (first vals)))
      ((eq? name (quote eq?))
       (eq? (first vals) (second vals)))
      ((eq? name (quote atom?))
       (:atom? (first vals)))
      ((eq? name (quote zero?))
       (zero? (first vals)))
      ((eq? name (quote add1))
       (add1 (first vals)))
      ((eq? name (quote sub1))
       (sub1 (first vals)))
      ; In a better version of this, here is where we would kick off the
      ; creation of a continuation. At this point, we'd probably extract
      ; a program counter, and probably some environment information.
      ; For memory, it might be enough to give the continuation the
      ; current table, then use that table again in whatever function
      ; gets returned to, because we don't have any set! functions
      ; implemented, nor do we plan to. The environment can't change
      ; without it.
      ((eq? name (quote call/cc))
       (call/cc (lambda (continuation)
                  ;(print-line 'function-argument-to-call/cc:) ;DEBUG
                  ;(print-line (first vals)) ;DEBUG
                  ; - first vals: first argument
                  ; - (second (first vals)): The lambda value inside of
                  ;   call/cc
                  (apply-closure (second (first vals)) 
                                 (list (build 
                                         (quote continuation) 
                                         continuation))))))
      ((eq? name (quote mul))
       (* (first vals) (second vals)))
      ((eq? name (quote add))
       (+ (first vals) (second vals)))
      ((eq? name (quote number?))
       (number? (first vals)))
      ;(else (print-line 'fell-through-apply-primitive) (cond) ) ;DEBUG
      )))



(define :atom?
  (lambda (x)
    (cond 
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) (quote primitive))
       #t)
      ((eq? (car x) (quote non-primitive))
       #t)
      (else #f))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry
               (formals-of closure)
               vals)
              (table-of closure)))))

; There should only ever be one argument to a continuation.
; In a better version of this, here's where we would extract the
; relevant information from a "more transparent" representation of a
; continuation, and then use that to perform a jump, while retaining the
; value passed to the continuation.
(define apply-continuation
  (lambda (continuation vals)
    (continuation (first vals))))

; Let Auxillary Functions: {{{ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define let-vars
  (lambda (e) 
    (map car (cadr e))))

(define let-vals
  (lambda (e)
    (map cadr (cadr e))))

(define let-body
  (lambda (e)
    (caddr e)))

(define lambda-from-let
  (lambda (e)
    (cons
     (list
      'lambda
      (let-vars e)
      (let-body e))
     (let-vals e))))
; }}} ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print-line
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
   5))

; Do we evalaute call/cc to a primitive?
(print-line
  (value 'call/cc))

; Can we evaluate a continuation as a function that returns normally?
(print-line
  (value '(call/cc (lambda (c) c))))

; What about when actually using the continuation?
(print-line
  (value '(call/cc (lambda (c) (c 10)))))

; What if we mix it with begin?
(print-line
  (value '(begin 1 (call/cc (lambda (c) (c 'successful-mix))))))

; Works! Recursion isn't the problem. Perhaps it's building lists
; recursively that's the problem.
(print-line 'evaluate-member?)
(print-line
  (value 
    '((lambda (function lst e)
        (function function lst e))
      (lambda (F lst e)
        (cond ((null? lst) #f)
              ((eq? (car lst) e) #t)
              (else (F F (cdr lst) e))))
      '(1 2 3 4)
      3)))

; This evalutes correctly, too. So it seems that the problem lies with
; my implementation of continuations.
(print-line 'evaluate-rember)
(print-line
  (value
    '((lambda (function lst e)
        (function function lst e))
      (lambda (F lst e)
        (cond ((null? lst) '())
              ((eq? (car lst) e) (cdr lst))
              (else (cons (car lst) (F F (cdr lst) e)))))
      '(1 2 3 4)
      3)))

(print-line
  (value 
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
      '(1 5 2 5 3 5 4 5)
      5)))

