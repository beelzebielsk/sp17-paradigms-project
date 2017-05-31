It seems that we start with the very last continuation to be called.
That's the first continuation passed. They all return to here. It's
called `(end-cont)`

Tail calls don't grow the continuation.

We don't yet know exactly what apply-cont does, which is why it is not
defined very well. We just know that it takes a continuation and a
value.

We'll need to create continuations for use with the language. The first
such builder is called `end-cont`, and calling it creates the final
continuation.

It's likely that end-cont will return a function, or a thing to which a
value can be applied. When we make the call:

	(apply-cont (end-cont) val)

The first parameter is the continuation returned by calling end-cont,
and apply-cont will apply `val` to the continuation returned by
(end-cnt). When this happens, the following expressions will occur:

	print some nice message.
	Return the value

When reading over value-of/k, we see that not every expression makes use
of apply-cont, but it seems to stick to: the evaluation of operands
causes continuations to grow. We see apply-cont

Continuations specify *how to finish a computation*.

Based on the zero? example, it seems that evaluations of things involve
creating a new continuation with a continuation builder that works for
that specific type of expression, which takes the current continuation
as an argument, and the continuations get chained. Basically, the
zero-cont continuation builder will probably calculate the right value
for the zero? function, then pass that value into the continuation given
to zero-cont, which will carry that value to wherever it needed to go.

So far, they're defining each of the different continuation types in
terms of what should happen when that continuation is an argument to
capply-cont. The different types are the 'continuation-builders' that
the book talked about, and it seems that every continuation builder
that's not the builder for the end continuation will take another
continuation as an argument.

Q: How will the continuations get passed to apply-cont? How does that happen?

I was wrong. Not all expressions will have continuation builders that
take *only* the current continuation, though I'm sure they all take *at
least* that much. They take in all the information necessary to return a
result when called with a single value as their parameter.

Some continuation functions take in the current environment. Since this
is implemented in SCheme, it's safe to say that it's lexically scoped.
Some continuations hold memory/state in that way, then. No 

Who's calling apply-cont? How do these continuations get their values to
do their thing? 

All of the thing-exp names refer to types of expressions. Maybe Scheme
defined datatypes.

Continuation builders may take many values, but continuations themselves
(so far) only ever take one parameter: a value to return to another
continuation.

This might clear up a bit how continuations get their state: they're
bound to them when the continuation is built, since continuations are
implemented as functions which are built by continuations builders. The
environment of these continuations is the environment of the
continuation builders, and these builders have some data bound to their
formal parameters. Those values are referrable by the continuation.o

The chain of continuations is a chain of calls to apply-cont. They
perform the calculation specified by the kind of continuation, then pass
that value into the previous continuation. There is never not a previous
continuation.

It seems like growing the continuation means making a new call to
value-of/k. When evaluation of a statement in the interpreter doesn't
cause a call to value-of/k, I think that means that there was no new
continuation formed. Rather, it really ought to be that there was no
function call to a continuation builder, but the two seem to coincide.

When you don't actually alter state from line-to-line, you don't
actually need more than one expression in a program. I felt the need to
include a print statement before demonstrating the behavior of call/cc
to prove to myself the order in which things were happening, but the
only reason that was useful was for its *side effect*, for altering the
state of the standard output file. Without this state alteration, the
line was pointless, and I could change state as necessary with nested
function calls, or nested lets.

In some sense, a destructive program could be written as a series of
nested let statements in scheme.

Q: Are tail calls really functional? Or are they technically
destructive? If you have a tail call, then you don't create a new frame
for a function call. When you don't create a new frame, you're alterning
the current frame. In doing so, you're destroying its old state.
Actually, the tail-call part is kinda unimportant. There's clearly an
element of time involved in a function call-- espcially if one considers
lexical binding. Function calls can "bring back the past"  using lexical
binding. So, what is it about the time surrounding function calls that's
okay?

Q: What exactly is the control context? Where does it start and end? I
have some basic intuitive idea, which seems to be deferred calls. The
other things that are going to happen-- which caused the current
statement/expression to happen. Context is details that are not in the
foreground, but are important to what's going to happen. The
environments are the data context: those cover how names get bound to
values/how names get transformed to values. So, what is the analogous
version of control for a control context?

# Guesses at how a Trampolining Interpreter Works

It's possible that a trampolining interpreter doesn't do much more than
just give a program access to the current continuation. When the current
continuation gets called, we end up going back with the passed value to
the current continuation's control context, not the control context of
wherever we were. This is because the control context is carried in the
continuation as part of its environment (the `cont` argument to any of
the continuation builders). So, if call/cc has a continuation builder,
it'll probably take a function of one argument and the current
continuation and pass that continuation into the function of one
argument. The application of that function will build up a control
context along the way, but if the continuation from call/cc is ever
called with a value, the value just gets sent to the control context
*of call/cc*. I still have to figure out just how this plays with
control flow-- like the mechanics of it. I get the general idea, but the
implementation details escape me a bit.

> Our goal is to rewrite the interpreter so that no call to value-of
> builds control context. When the control context needs to grow, we
> extend the continuation parameter.

I think they mean that value-of doesn't extend control context in the
underlying scheme. All growing/shrinking of control context happens
entirely within the continuation parameter. If they grow in the
underlying scheme, then they're cheating. They haven't made control
contexts transparent. In TLS scheme, we'd have an environment, and the
current continuation, which holds the control context, would ultimately
be placed within a function's environment. The control context will show
up in the environments of the continuations. Each continuation gets one
value from its formal parameter and everything else is placed in the
continuation's environment when the continuation is created, including
the control context as a parameter of the environment (the current
continuation).

While the value-of function of the interpreter will still be recursive,
it will be iteratively recursive, because we won't require the meaning
of another statement more than once in a single expression in the
underlying scheme. Th meanings of expressions will be encoded in
continuations by continuation builders.

Anything in normal TLS Scheme whose values/meanings depended upon the
values/meanings of subexpressions had to extend the control context of
the underlying scheme, because the subexpressions had to be evaluated.
What we should see in TLS Scheme with continuations is the creation of a
new continuation whenever this would happen.

Notice that if we called an earlier continuation, we wouldn't return to
any statements that surrounded where the continuation was called. The
interpreter is not classically recursive, so it's not going to
eventually return to those calls that surrounded the use of the
continuation. It just follows the chain of operations that was built up
in the control context. That seems to be the magic of the continuation:
call/cc, which allows one to sneak in a function with a much smaller
(earlier) control context, and allowing one to execute that. That is
also why the chain of deferred calls can be "resurrected", because they
weren't in a stack in the first place. The continuation itself held them
as a chain of operations.

Thinking of continuations in this way makes sense of some of the stuff I
saw for rewriting complicated expressions in terms of continuations.
You'd have to write them in terms of the chain of function calls that
would occur.

	(+ 1 (+ 2 (* 3 4)))

This would probably end up as:

- See the `+`, start up a continuation for adding.
- See the first actual parameter, set up a continuation that takes the
	second actual parameter and adds them together.
- Going after the second actual parameter, you see another application.
	You pretty much repeat the same process here, creating more
	continuations to handle the operations, passing a single value at a
	time backward. I think the chain would sorta be like:


	Return a sum 
	<- return the sum of 1 and second arg = (+ 2 (* 3 4)) 
	<- return a sum
	<- return the sum of 2 and second arg = (* 3 4)
	<- return a product
	<- return the product of 3 and second arg = 4
	<- return a number, 4

You evaluate this starting from the deepest level, which is:

- Returning the value of 4.
- Taking the product of 3 and 4 and returning that to the previous link.
	The 3 was in there as part of the environment.
- Returning a value, the product from the continuation.
- Take the sum of 2 (which was kept in the environment) and the value
	returned from the product ( `(* 3 4)` = 12), which is 2 + 12 = 14.
	Return that value to the previous link.
- Return the returned value to the previous link.
- Return the sum of 1 (which was maintained in the environment of this
	link) and the value given to this link, which was 14. The sum is 1 +
	14 = 15.
- Return 15.
