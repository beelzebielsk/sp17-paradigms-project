A project to implement continuations and understand their uses.

`scheme-with-continuations.scm` implements an interpreter on top of R5RS
scheme. The interpreter is written in a *Continuation Passing Style*.

Implemented:

- Continuations
- `call-with-current-continuation` (named `call/cc`, in this interpreter).

In Progress:

- Error handling
	- There is an error data-type, and errors are produced by some
		syntactically correct expressions that have no valid meaning.
		- Applications whose first element is not a function.
		- Identifiers with no bound value in the environment.
	- There is a `try` form which takes in a `try` function and `catch`
		function, where the catch function will catch errors produced by by
		the try function.
