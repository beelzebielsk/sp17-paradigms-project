Introduction
============

You introduced continuations to us, and I decided to choose
implementing continuations as my project. Generally, I feel like I
need to understand how a tool works in order to use it. When I build,
I like flexibility in the way I use my tools, which means that I need
to know their limitations, which means that I need to know how the
tool works.

Our first attempt at creating continuations was to simply place a few
expressions in the TLS Scheme language that allowed someone using the
language to access continuations of the underlying scheme.

This was... a cheap imitation of continuations. No real understanding
of continuations was necessary to implement this, and the criticism we
received was that an implementation of continuations being done this
way might not even be correct. I don't fully understand why that's
true, but I think I understand part of it. The part I understand has
to do with the definition of the continuation, which is...

DRUM ROLL

What is a Continuation?
=======================

A continuation is a way of keeping track of the execution context of a
program. The languages that I've used in the past use a stack to do
this.

The execution context of a program is what a program is going to do
when the program is finished doing what it is currently doing. It's
information about execution that is not known from the current
expression being evaluated (similar to how an environment could be
considered *data context*, as the environment keeps track of
information that is not in the current expression being evaluated).

Why Would We Care about Continuations?
--------------------------------------

I think the point of continuations is to make function evaluation more
functional. If you track the execution context with a stack, as is
done in a computer, and is also done in languages like C, then there
is this global monolithic object which keeps track of information that
is relevant to evaluating a program, but this information is not
actually a part of any of the functions getting called.

The point of functional programming is to try and make sure that
functions work like mathematical functions: all the information
necessary to evaluate the function is in the function's arguments.
Since the execution context influences what would happen in general,
it's relevant information to know when evaluating a function. You can
still get the meaning of the function itself, but not the meaning of
the whole program. The meaning of the whole program changes if you
change the execution context.

My Understanding of Continuations
===================================

We are taking the control execution out of the underlying machine and
instead creating a data structure to contain it

Steps required To make TLS Scheme use Continutions
==================================================

Messing with Continuations: Implementing Exception Handling
===========================================================

Future Work
===========

Can I implement continuations in a traditionally stack-based language?
I think something like it is possible, though the implementation would
be slightly different. My idea would be, rather than creating
continuations and passing them as arguments into each function
(behind the scenes) treat continuations as a generalization of a
stack: a stack just grows upward. Replace the stack with a tree, and
instead of creating and passing the continuations themselves, create
and pass pointers into the tree. I figure that continuations allow you
to switch to arbitrary control contexts by saving a continuation out
and using it, but (to my knowledge) you can't *manufacture* a
continuation on your own. All the continuations that you'd use are
continuations that must've existed prior. You can also return to a
previous point and go create new continuations from there. I'm pretty
sure that a stack-tree could capture all the different sorts of things
that could happen with creating continuations. I'm just not sure how
to allocate these frames in memory, and how to clear them. If the
stack doesn't grow linearly, then one might jump all around in memory.
