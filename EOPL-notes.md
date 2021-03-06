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

Are environments bound to continuations? Probably, because the
conntinuation is built during a call to value-of, which has an
environment as a parameter, and thus must grab that environment as it's
own lexically bound environment.

# First Attempt at Building a CPS Interpreter

We don't want the meaning function to build up control contexts. So, for
one, we might not be tagging data like functions anymore. The knowledge
of what to do with the function will likely be encoded in the
continuation, rather than figured out later once all of the expressions
have been evaluated, then passed to some application function.

I don't have the "cases" function, so the agreement that I will honor
is:

- The meaning function will not open up new frames for meanings, though
  new frames may open up as a result of finding the right function to
  handle the current expression. Either way, the number of frames that
  will ever open up ought to be finite: the longest path through the
  `expression-to-action` function to a resulting action function.
- No action function should open up a new frame, except as a result of
  looking up variable names. In this case, the number of frames that
  shall be opened up will be finite: no longer than the number of ribs
  in the rib-cage environment.

Since the meaning and action functions use a bounded amount of frames,
they should be *iterative*.

I thought that having environments of arbitrary depth might mean that
our interpreter builds control context (to an arbitrary degree), and
that this would break my contract of making the underlying scheme build
only a finite amount of memory at any one time, however I was wrong on
several points:

- What matters is that the underlying scheme uses a bounded amount of
  new continuation frames, not memory. The program could get arbitrarily
  long, which would break the finite memory contract in a way that
  doesn't make sense.
- The lookup functions ultimately are iterative. There's no necessary
  knowledge of anything that came before when we look through them. We
  just have calls to lookup in an entry, each of which looks through
  name/value pairs of the rib in an iterative manner. At the end of each
  lookup in entry function, we call a callback to lookup in the ribcage
  with that rib removed. This is, again, iterative, because the result
  of the callback is the result of the function. There is no need for a
  new continuation frame.
- The sorts of calls that would build up continuation frames are things
  like:
  - The original `*application`, which evaluated a function, then
    evaluated the list of arguments to the function, then passed those
    to an `apply` function. The evaluations necessarily built control
    context, and could've done so without bound, as each of the
    expressions to evaluate could've been arbitrarily complicated with
    subexpressions, and the evaluation of subexpressions required
    extensions to the control context.
  - `evlis`, which built a list of meanings from a list of arguments.

I think you end up adding continuation frames when you've got "more
things to do". Basically, you add a frame when right now is a place that
you'll eventually return to with a value, but not yet. You don't add a
frame if you get get a value right now.

Note that there's a difference between extending the data context
(introducing new bindings) and extending the control context. A
function call in the meaning function can introduce new bindings in an
environment, there's nothing wrong with that.

However, the bindings of names have thus far been computing values: the
meaning of such values were already computed (our interpreter was
eager). So, forcing the interpeter to evaluate such things would
completely undermine out building of continuations.

Doing expressions like numbers as not extending the control context was
not wrong. They shouldn't and the book doesn't make them do so either.
Because when the number is not an operand, then there is no reason to
extend control context. That means, that the control context is extended
when something that is in operand position is located.

# Continuations as a Data Structure

I think continuations are an example of what the book calls *procedural
data*. Instead of creating functions to deal with datatypes, extracting
information from them and such, the data structures are functions which
return the only interesting value to extract from them. The actual data
is maintained in the function environment, so you leverage the
function's data structure as your data structure.

I think they can do this precisely when the data type only has
constructors and a single accessor. If every method of creating certain
data requires building data (if it's not mutable, pretty much), and
there's only one thing you'd want to do with the data outside of
creating something new (one accessor), then you can make the data into
the accessor function itself and figure out constructors that can
leverage this, which return new functions out of the old function and a
few other parameters.

Well, If you think about continuations, you have the continuation itself
and the continuation builder. The only things you'd want to do with
continuations is:

- Extend the control context by creating a new continuation out of the
  current continuation, and packing any required information into the
  new continuation by way of free variables.
- Apply an argument to a continuation, causing your function to
  "return".

The continuations are essentially stacks. Passable stacks. Just as a
stack keeps track of control context in C/C++, continuations do so too,
but they maintain the stack in the environment of functions, which can
be passed around. In C/C++, control context exists outside of statements
as part of the program's state, and instructions can only modify this
piece of global program state.

They're stacks because new continuations are added on top of old
continuations, and the continuation that will get applied first is the
continuation at the very "top". More in terms of the recursive structure
of the continuation, the current continuation gets applied to the
argument, which in turn produces a value, which gets sent to the
previous continuation (the continuation that was current at the time
that the current continuation was being created). That order is a LIFO
order, and thus it might as well be a stack.

It's the evaluation of *operands* that should cause the expansion of
control context. Operands only occur when there is an operation.
Therefore, expansion of control context really ought to occur inside of
applications, but *not due to the application itself*, but rather the
evaluation of the operands within that application.

# Proving that my CPS Interpreter "works"

How would I go about proving that a CPS interpreter is equivalent to the
other interpeters that I've been working with? I'm sure part of the
answer to this question is the fact that the continuations are an
inductive data structure. There is a smallest such strucutre, and ways
of building larger structures from smaller structures. So, how would I
go about this? I suppose I would start with expressions with a given
length in the chain of continuations and show that all of those are
equivalent. I'd start with expressions that open up no new
continuations: primitives, lambdas (I think that's it. Let forms should
open up new continuations for evaluating the a-list, and applications
should open up new continuations for evaluating the operands). Then, we
investigate expressions whose control context does not exceed n calls.
What would I need to do here? Would I have to show that the chain of
calls is both *built correctly* and *terminates correctly*? How would I
break up building correctly into smaller pieces?

I also need to figure out the exact meaning of 'works'.

- When passed well-formed expressions
- Accepts some subset of the valid expressions of the underlying scheme?
  (It's not all of it, but what happens if I decide to extend the
  underlying scheme? That's a different proof in this situation)


# Some of the Book Specific Functions for defining Languages:

- **`cases`**
  - Found on page 72
- **`define-datatype`**
  - Found on page 70
  
# My understanding of continuations after implementing them

They're not too much unlike a stack. I'd go as far as saying that a
stack is a special case of a continuation.

The continuations hold the "context of control", much like a stack does.
It holds knowledge of "what to do next", when you're done with the
current piece.

An interpreter is very recursive, so long as there are any expressions
which are built out of smaller expressions. Rather than forcing the
underlying machine to handle all of that recursion explicitly, by
relying on the underlying machine's method of creating control context
(which is probably a stack), you explicitly define the control context
as something called a continuation, and you make that continuation a
parameter that the interpreter uses.

So, for instance, take the task of evauating the following scheme
expression:

~~~ {.scheme}
(cond (#t 0)
      (#f 1))
~~~

This is an expression that is built out of several subexpressions.
To evaluate it, you have to step through a list of question-answer
pairs, figure out if the question evaluates to true, then return the
answer.

Normally, from the point of view of the underlying machine on which the
interpreter is built, you'd do something like:

1. Take the first question and answer pair. Take the question from it.
2. Evaluate the question, and upon getting the value back, check if the
   value was true.
3. If the value was true, then return the value of the answer.
   Otherwise, evaluate the rest of the question-answer pairs.

Here, the underlying machine goes to call the meaning function first,
while still in the function call that's evaluating the entire cond
statement. Then, upon getting a value, it takes one of two paths to the
end of the function call to evaluate the cond. This would be a properly
recursive call, because all of the state of the current function has to
be saved, then resumed, upon getting the value from the recursive
evaluation call for the question.

With continuations and continuation passing, you'd evaluate things like
this instead.

1. Take the first question and answer pair. Take the question from it.
2. Rather than making use of the underlying machine's control context to
   recursively evaluate the question, you create a new continuation:
   - I don't know what the value of the question is, but I know what to
     do when I do get the question's value: I should return the value of
     the answer if the question's value was true, and evaluate the rest
     of the cond otherwise (if there is anything left).
   - So, you create a function which does just that: you give it the
     answer, and the rest of the cond which might need to get evaluated,
     and it produces a new function which takes just one argument: the
     value of the question. 
   - You issue a recursive evaluation call on the question, just as you
     would have done before, but it is no longer properly recursive from
     the underlying machine's point of view. The meaning of the cond is
     whatever will come out of your new continuation, once the value of
     the question gets passed to it. So the underlying machine has
     nothing to remember: all of that information of what to do, and
     data that's needed to know what to do was placed in the
     continuation instead.

There's one thing that I left out. It's more than likely that the cond
statement was surrounded by other statements. Even if it wasn't, there
is always a continuation that you start from, which is called the *end
continuation*, which, when carried out, is the end of the program. So,
creating new continuations also requires the old continuation.

This is technically like a stack: the environment of the continuation
functions stores information that the function needs to execute, as well
as the previous continuation to return a value to, once this
continuation can create a value. This is basically like a return address
and function parameters being stored on a stack.

This is nothing special by itself, until you allow these continuations
to become data that your interpreter can make use of. That's where
things get strange.

Stacks have to unwind for a program to run properly. Function calls that
get made have to return.

Here, unwinding is following the chain of created continuations from the
current continuation to the first continuation (end continuation).
However, each of the continuations can unwind to that, and so the
program can end correctly so long as it has a valid continuation.

So giving the language access to continuations is much like giving a C
program the ability to copy and swap out the stack with other snapshots
of the stack. You can jump to a stack from an earlier call, thus
circumventing many function calls. If you maintain continuations that
different function calls can see, and allow the two different function
calls to change that continuation, then you can even have access to
*entirely different stacks*. And since these stack snapshots are values
in your language, you can pass them to functions. So a function can
*choose where it returns to*, because it can choose which stack is going
to be the stack snapshot that's in place when it returns.

Continuations are not a process snapshot, because we're not copying all
of the memory of a program. The only memory that we're copying in a
continuation is the state of the stack. No work gets "undone" (unless
you wrote that work into the stack, in which case, bummer).

While a continuation sort of acts like a jump, that's not truly the
case. A jump moves a program to a certain line of the program. A jump
can manuever a program lexically (as in moving to a line of code), but
it can't maneuver through a program's *time*. You can't jump to a
previous *function call*, even though you can jump to a function.

Using a continuation can "jump" you to a previous function call,
however, by moving you to the right line of the function, and then
reinstating the stack that was around at the time that the continuation
was created. All the same return addresses are there (in the case of
Scheme, the continuation still holds the old contiuation in its
environment, and that old continuation holds its old continuation...,
and so on until we get to the end continuation).

I say "jump" rather than jump because a normal jump changes what happens
next by altering a program counter. Using a new continuation is just
using a different but valid choice for returning to the end
continuation. It's not a jump in the same way that using a value that's
alive, but not in the current scope isn't jumping back to the old scope.
You're just using something from before.

In general, a building a new continuation from an old one is *not
necessary* if the evaluation of the current expression does not require
properly recursive calls in the underlying machine. Note that recursive
calls to meaning are fine, so long as they're not *properly recursive*.
That means that the meaning call is going to be the return value of the
current function. That's fine, because then all the information of the
current function can just be forgotten, and you use the current meaning
function.

Not using the underlying machine's control context for the evaluation of
expressions is not about "not cheating" or just some challenge to stroke
the ego. Any time we depend on it for evaluating the meaning of an
expression, any information about evaluating that expression is now
trapped in the underlying machine. To make matters worse, if it
happened, then our program wouldn't end correctly. It would behave like
this:

- Save the control context in the underlying machine in a properly
  recursive call to `meaning`.
- Proceed with continuations through the rest of the program.
- Finish evaluating the end continuation.
- Then, after the program *should have ended*, return *nothing* to the
  properly recursive function call to `meaning` from the 1st step.

New continuation not necessary:

- primitive data: Evaluating these requires just one call to a defined
  function in the underlying machine.
  - numbers
  - booleans
  - primitive functions
- "expression-to-action": Not necessary, because we know that this will
  return after a finite number of steps, all the time (with a
  well-formed expression). It's not so much the case that we can't use
  the control context of the underlying machine *at all*, but just never
  for the use of evaluating an interpreter expression (calling the
  `meaning` function).
- `quote`: Not necessary, because the value of `(quote exp)` is `exp`,
  which is a value that can be obtained without issuing recursive calls
  to meaning, at all.
- `lambda`: Nothing in the lambda form has to be evaluated in order to
  return a lambda. The return value of a lambda statement is just a
  re-arrangement of all the information in the lambda statement, along
  with the current environment from the interpreter.

New continuations necessary:

- `cond`: To figure out the value of a cond statement, we have to figure
  out the value of the question, *then* figure out the value of
  something else. There is no way to do this in the underlying machine
  without being properly recursive. The value from a call to the meaning
  function is necessary in order to know what how to evaluate the cond.
  Instead of doing this in the underlying machine, create a new function
  which contains all of the necessary information for doing the next
  thing:
  - The rest of the unevaluated pieces of the cond (the other
    question-answer pairs)
  - The answer for the current question you have to evaluate
  - Where to return the value of the cond (the current continuation).

  That function is a new continuation. It has *extended the control
  context*.  Then, the last line in the function for evaluating cond is
  just a call to meaning without anything surrounding it. You evaluate
  the meaning of the question and pass that meaning into your new
  continuation which will use that value to decide whether to return the
  current answer, or evaluate the rest of the cond.
- `if`: The same case as the cond statement. To find the value of the
  if, you have to find the value of a question and an one of two
  answers, which could not be done in the underlying machine alone
  without a properly recursive call to `meaning`.
- `applications`: In order to find the value of an application, you have
  to find the value of all the expressions in a list, then, since the
  first element of the list should be a function, apply that function to
  the rest of the list. That requires a properly recursive call, because
  knowing the value of an applicaiton requires knowing the value of
  *several* statements first, and then those values have to be combined
  in some way. Thus, instead of trying to do this recursively in the
  underying machine, we create a new continuation out of the current one
  and pack all the information that's needed to figure out what to do
  next after evaluating the first element of the list of elements in the
  application:
  - Give it the old continuation, so that it knows where to return.
  - Give it all of the other elements from the list that it will have to
    evaluate (if there are any)
  - Give it any arguments that you've evaluated so far (if there are
    any)
  - Give it the current environment, in case it needs to find another
    meaning from a thing in the application list (which it probably will).

  You take the burden of remembering what to do when the underlying
  machine gets the value of the current element of the application list
  and pack all of that information into a new continuation instead.

# Improved Understanding

## Explaining Continuations relative to Stacks

most of the languages that we use have functions which can return
values.

For this to work, a program needs to have a *control context*. A control
context captures the information that's necessary to alter the flow of
control. In a standard C program, the control context is the stack. The
stack captures all of the necessary information to be able to say "I
have to do something else first to get a value, then continue from where
I left off with that value". In a C function call, the current location of
the program (the Program Counter) is stored on the stack, then a
function is called. When the called function is finished, it places a
return value in a location where the calling function can get it, then
the program jumps back to the location that was stored on the stack.

In addition to keeping control information on the stack, function
information is kept on the stack, too, like parameter values.

*Continuations* are another way of describing control context. They
maintain a place to return to, as well as function information; they're
not unlike using stacks. I'm not sure how many different ways one can
implement continuations, but my implementation uses closures. The
function information and return location are stored in the environment
of a closure, and the closure itself takes the return value from
whatever function is called.

So we have something analogous here:

- Agreed upon location to place a return value:
  - C/C++/stack based compilers/interpreters: Some previously agreed
    upon location on the stack, or an agreed upon register from the
    processor.
  - Continuation Based compilers/interpreters: The return value is
    passed in as a parameter to a function which represents the control
    context.
- Information Stored in both:
  - Parameter information
  - Place to return to

When information about control flow needs to be stored in a stack-based
program, a new frame is created on a stack which includes a return
address and leaves space for a new function to store parameter
information. When information about control flow needs to be stored in a
continuation-based program, a function is created that contains the
following:

- The current continuation, because when the current action is finished
  running, it needs to return.
- Any information the current action would need to finish what it was
  doing (any variable/parameter values).
- Information on what to do to produce the correct value once the action
  of the "called function" would take place. There's *a lot* that's wrong
  with this previous sentence, but I don't want to delve too far into
  how continuations and stacks differ yet. The best way to think about
  this: if, in a normal stack-based language, a function call would
  return a value to the caller, and the caller would continue on with
  that value, in a CPS style language, all of the instructions that
  would come after the function call are moved into a new continuation,
  along with any of the information those instructions would need to
  run correctly.

When In much the same way that a stack "unwinds" as values
are returned to caling functions, one travels up a chain of
continuations.

## Where They Tend to Differ

Continuations, by themselves, aren't much different than stacks.
However, the stack is a single, global monolithic data structure that
everything touches. Continuations, on the other hand, are *values*.
Building them well typically means creating an interpreter in
*Continuation Passing Style* (CPS). Much like interpreters from Scheme and
Scheme-like languages pass around an *environment* (a data structure
which contains mappings of names to bound values), a CPS interpreter
also passes around continuations.

There's already a difference here, in that continuations get *passed*
during the compilation/interpretation of a program, while the stack is
a global object that is in one place all the time.

Furthermore, continuations differ in when they grow. Continuations only
grow when there is a need to do something properly recursive: when some
other course of action has to be completed first before the current
course of action can be completed *which would not complete the current
action*. The stack grows with function calls. Continuations don't
actually grow with function calls. The function will return it's value
wherever it needs to grow. However, evaluating a function call means
evaluating a reference to a function, and the function's parameters.
Each of *those* requires opening a continuation, because the value of a
function call is not the function's value, nor the value of any of the
parameters (in general), but a combination of those things. The
evaluation of the function's body with the actual parameters bound to
formal paramters doesn't create a new continuation, because this value
would just go wherever the value of the function call would go; the two
values are one and the same.

- The evaluation of operands cause the creation of new continuations,
  not the evaluation of functions.
- The evaluation of tail call functions do *not* cause the creation of
  new continuations, because no new control information is necessary.
  The value of the tail call goes exactly where the value of the
  function would go, because the value of the tail call is the value of
  the function.

This is why that sentence that implied that continuations grow with
function calls is very, very wrong. They only grow when evaluating an
expression whose value is not the value of the current expression. Doing
this is necessarily *properly recursive*, because evaluating while
evaluating is recursive, and because the evaluation of the original
statement won't be finished after the evaluation of this new statement,
information needs to be saved on where to come back with the value of
the new statement.

However, they differ even more because, since a continuation is a value,
you can allow your language to use these values, in much the same way
that a language like Scheme has first-class functions. So continuations
provide a way of cleanly and easily manipulating the control context of
a program.

Doing something similar in a stack-based program would require
maintaning copies of the stack, and replacing the existing stack with
the copy. That could be dangerous, and it certainly wouldn't be easy.

## Implementing Continuations

The language that I created is implemented on top of the high-level
language R5RS Scheme. The function that evaluates statements is called
`meaning`. While there is a `value` function defined in the progra which
is used often, it is just a front-end to meaning. The bulk of
implementing the continuations is doing two things:

- Altering the `meaning` function to take a continuation as an argument,
  in addition to an environment and an expression to evaluate.
- Taking any properly recursive call that the underlying Scheme would
  make and expressing that call through continuations.

The first and second piece are really all part of the same goal. To give
the interpreter itself complete control over its control context, you
have to take all of the information about control flow and make it
completely accessible to the interpreter you're building. Scheme doesn't
come with tools for evaluating its control context, so maintaing *any*
control context in the underlying Scheme machine means locking needed
information away in a place where the interpreter can't reach it.
Putting these continatuions into the `meaning` function means they're
accessible to the meaning function.

### Where Continuations Start

Control contexts ultimately start at the end, because they are LIFO
structures. When the first stack frame is reached, you are likely at the
end of the program. Similarly, the first continuation of a program is
called the *end continuaiton*. It is a function that specifies what to
do when your program finishes evaluating.

~~~ {.scheme}
(define end-cont 
  (lambda (value)
    (display "The value of the expression was: ")
    (print-line value)
    (print-line '(End program))))
~~~

### When Continuations Grow

In my language I have several different general types of statements:

- Primitive expressions, which are either just a primitive data type
  (number/boolean), or a primitive function name.
- Resolution of an identifier, for any name that is not the name of a
  primitive function.
- And some 'list forms':
  - `(quote exp)`
  - `(cond (q1 a1) ... (qn an))`
  - `(if question if-true if-false)`
  - `(lambda (param1, ..., paramn) body)`
  - `(let ((name1 val1) ... (namen valn)) expression)`
  - `(function-value arg1 ... argn)`

Primitive expressions, resolutions of identifiers, labmda expressions,
and quote forms do not require the extension of control context.
Evaluating these expressions is either a single non-recursive function
call (or recursive call with finitely many calls, none of which contain
information about the control flow of the interpreter) from the
underlying Scheme, or in the case of quote, the value of the expression
is the expression itself.

However, the `if`, `cond`, `let`, and applications of functions all
require the use of continuations.

`if` and `cond` are very similar, as are applications and `let`. I will
cover just one situation from each pair.

### Cond

There are two forms of `cond` which require no extension of control
context:

- `(cond)`, which produces nothing.
- `(cond (else exp))`, whose value is just the value of `exp`, and the
  interpreter will see that.

All other forms of cond will require a continuation, because evaluating
the `cond` staement means evaluating one of the questions, and then
doing something based on the value of the question. This is a properly
recursive function call, and as such, information about this call needs
to be stored in a continuation. This continuation requires:

- All other question/answer pairs of the `cond`, just in case the value
  of the first question is false.
- The answer to evaluate in case the value of the first question is
  true.
- The current environment, for evaluating the answer, or to pass to the
  function that will evaluate the rest of the `cond`.
- The current continuation, because once this new continuation finishes
  it's job, it must produce a value to return, and it must return that
  value wherever the value of the `cond` should have gone.

~~~ {.scheme}
; The function which states what to do when a cond expression is to be
; evaluated.
(define *cond
  (lambda (environment expression continuation)
    ; Remove the 'cond'. Now we just have a list of questions and
    ; answer pairs.
    (evcon environment (cdr expression) continuation)))

; The function which states *how* to evaluate a cond expression.
(define evcon
  (lambda (environment qa-list continuation)
    (let ((question (caar qa-list)) (answer (cadar qa-list)))
      (cond ((eq? 'else question)
             ;(print-line "Question was 'else.") ;DEBUG
             (meaning environment answer continuation))
            (else
              (meaning environment 
                       question 
                       (cond-cont environment 
                                  answer 
                                  (cdr qa-list) 
                                  continuation)))))))

; The function for creating new continuations for cond expressions.
(define cond-cont
  (lambda (environment answer rest-of-cond old-cont)
    (lambda (value)
      (if value
        (meaning environment answer old-cont)
        (evcon environment rest-of-cond old-cont)))))
~~~

### applications

The simplest possible application is `(function-expr)`, which is a
function call for a function with no arguments. Even this requires a
continuation.

In order to evaluate the application, the `function-expr` has to be
evaluated first, then the evalation of the application has to continue.
Thus, an extension of control context must occur.

If there are arguments to evaluate, then those too must cause expansions
of control context.

The two situations are essentially the same, because they are all the
evaluation of expressions, so I use the same continuation building
function for each. In order to continue evaluating the function, the
following information is necessary to give to the continuation:

- The value of all the expressions of the application left to evaluate
  (if there are none, then a value is passed which indicates that; in my
  implementation, it's the empty list).
- The value of all the expressions that have been evaluated so far, if
  there are any. If there are none, then a value is passed which
  indicates that; In my implementation, that is the empty list. This
  part is necessary because these continuations for function evaluation
  essentially collect a list of the evaluated expressions until all of
  the expressions have been evaluated. The process is done through
  continuations so that there's no properly recursive function call in
  the underlying Scheme.
- The current continuation, because if the application is to be
  evaluated, it must return its value to the current continuation.
- The current environment, for evaluating expressions.

~~~ {.scheme}
(define *application
  (lambda (environment expression continuation)
    (print-line "Went to application.") ;DEBUG
    (meaning environment 
             (car expression) ; The function of the application.
             (eval-op-cont environment 
                           (cdr expression) 
                           (list) 
                           continuation))))

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
                (let ((function 
                        (get-func-from-params-evaled params-evaled))
                      (params 
                        (get-params-from-params-evaled params-evaled)))
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

(define apply-func
  (lambda (environment function params old-cont)
    (print-line "Went to apply-func.") ;DEBUG
    (cond ((primitive? function) 
           (apply-primitive environment function params old-cont))
          ((non-primitive? function) 
           (apply-nonprimitive environment function params old-cont))
          ((continuation? function)
           (apply-continuation environment function params old-cont))
          (else (no-function-of-type)))))
~~~

The actual implementations of `apply-primitive`, `apply-nonprimitive`,
and `apply-continuation` can be found in the source code, but they're
unimportant to our discussion, because none of them ever create new
continuations. They can readily evaluate the expression that they need
to evaluate.
