# Implementing Exception Handling

To implement this, I'll have to implement error generation first. I
think the fastest way to do that is to implement a new data-type. It
will be a compound whose tag is `'error`. It'll carry some relevant
information describing the error, like an error message or a trace.

I think that the best way to do exception handling is to create a `try`
form. It will have the following form:

	(try try-func catch-func [finally-func])

The try form will create a new continuation which receives the meaning
of the `try-func`, and if the value is not an error, then it will pass
the returned value to the normal continuation, otherwise, it will pass
the error to the `catch-func`, which will receive the error as an
argument and evaluate itself. Then, it will continue from wherever it
should've continued, so I suppose it also needs to have the old
continuation, and it should send its return value to the old
continuation.

TODO: Look up how exception handling is typically done in functional
languages.

# Implementing Threading

I wonder? If I implement threading, doesn't that mean I need to
implement a scheduler? Would all normal programs end up becoming a
special case of threading where there's only one thread? 

# Figure out how to implement this at an assembler level

- Am I going to have continuation objects, whose address I pass around,
	or one global "context tree", where pointers into this tree are the
	continuations?
	- If I do a tree, how will I allocate/deallocate space for new parts
		of the tree? I'd like to be able to do the equivalent of "move stack
		pointer to base pointer to deallocate all of the stack on top of
		this point". It would work *really* well with continuations,
		considering that we can jump back and forth arbitrarily through
		context by using a different (usually earlier) continuation. I also
		like the idea of a tree because I think it would be easier to slide
		in this implementation to something like C/C++ which is stack based.
		Most of the semantics wouldn't change at all, but I'd leave room for
		the stack to grow in different ways than linearly, much like I think
		continuations do.
- What is going to work with these continuations? Am I going to try and
	plug it into C/C++? Am I going to design a language from the assembler
	level? Am I going to write a, what, an assember API to a language
	designed elsewhere? How do languages like C/C++ get programmed anyhow?
- What are the tradeoffs going to be? Where is this going to get slower
	than traditional stack-based stuff, and where is this going to get
	faster? How much more memory am I liable to use with an implementation
	like this? What sorts of problems can arise from allowing their use?
	Continuations are, in a sense, an even scarier form of goto (or are
	they actually *simpler*, because they carry enough information on them
	that you *know* to where/when you've jumped?).

# Immediate Program Exit

Just creat a procedure called `exit` that ignores the current
continuation. It just passes it's one argument into the `end-cont`.
