On Process Hierachies
=====================

I have been in thinking mode the last couple of days. It all started
when I decided to look at the problem of *wrong* in
haskell-torrent. What will happen when something fails? Joe Armstrong
has part of the answer by arguing that we must proactively expect a
process to fail and then let some other process clean it up
(paraphrased a lot by me, these are not his exact words).

Take a look at this image,

{}

It is a [hypergraph](http://en.wikipedia.org/wiki/Hypergraph) where
each vertex corresponds to a process and each channel corresponds to
an edge. The graph is undirected because in principle each process can
both send and receive on the channel in question. In practice however,
one can limit the communication as some processes will only transmit
on a channel, and others will only receive.

To simplify the graph, I omit local RPC channels. It is customary to
implement RPC by creating a local channel and transmitting that
channel over one of the hypergraph edges. Response can then work
directly on the local channel, simplifying the problem of giving
*identity* to processes in many cases.

Termination
-----------

A scary part of such a network is what happens when we want to
terminate it or if something goes wrong in one of the processes. In
particular, we have the problem that while Haskell is a pure, nice,
protected Nirvana -- the real world inside IO is a dank, dark place of
twisted little passages (all alike). Thus it might very well be that
we get to experience an exception first-hand in one of the above
processes.

I have toyed with the idea of copying the excellent CHP library by
Neil Brown. Neil defines a concept of *poison* for channels. A channel
that has been poisoned will always transmit poison. Poison thus
propagates through the hypergraph and slowly but surely kills off each
process. Neil has a description of the process
[on his blog](http://chplib.wordpress.com/2009/09/30/poison-concurrent-termination/). I
like the idea because it uses the existing infrastructure of the
hypergraph. It would be fairly easy to add support for writing poison
handlers at each node.

Process Hierachies
------------------

I have pondered on a different scheme for the last couple of days
however. We can impose a *location graph* on the hypergraph above:

{}

This graph, a tree, describes the location or hierarchy of the
processes in question. The processes named S0, S1, ... and so forth
are *supervisors*. Their responsibility is only one: Keep their
children running according to a *policy*. Erlang programmers fluent in
the OTP high level libraries present in the Erlang distribution will
recognize the supervisor term.

A normal (*worker*) process has two rules:

  - If the process terminates, it must inform its supervisor.
  - If the process gets an asynchronous KillThread exception, it
      *must* die.

A supervisor process needs to obey the following scheme:

  - If a process under its supervision dies it must take affair
    according to its policy. This may mean that it kills off all
    other children and reports back to its supervisor for
    instance. Or it may mean that it simply restarts the process
    that got killed. What happens is dependent on the policy we
    configured the supervisor with.

  - If a supervisor is asked to shutdown by a KillThread exception,
    it must first kill all of its children.

Note that with these rules, termination is almost free: Terminate the
tree. Because everything is linked to the tree, termination will
happen. We will terminate by asynchronous exceptions aggressively.

An interesting difference from Erlang is that of communication, which
is a certain sense is Dual in Haskell+CML: In Erlang,
the process id is what you need to hold in order to transmit an
(async.) message to the process. In our CML-based system the *channel*
is the name you must hold in order to communicate. In other words,
this lets us, in some circumstances, replace the process with a new
one but keeping the channel. Note, though, that the interface is not
completely clean: if the Haskell runtime figures out a thread is
indefinitely blocked, it will in general throw an exception -- but I
have not yet read the CML code in detail so I do not know if this will
happen. It depends on what MVar references a channel hold.

Another view
------------

The hypergraph and location graph constitutes what is sometimes called
a *Bigraph*. This is not to be confused with the term Bi*partite*
graph which is a completely different term but sometimes also called a
bigraph unfortunately. I somewhat resist the idea to use the same term
for different concepts, but
[work](http://www.itu.dk/~mikkelbu/research/bigraphsbib/index.html)
done in this twi-graph concept presented in this post on
[bigraphs](http://www.itu.dk/research/pls/wiki/index.php/A_Brief_Introduction_To_Bigraphs)
suggests that the term is quite used in this respect.

The research seems different from what I need
however. [ITU](http://www.itu.dk) has done extensive research in
bigraphical programming languages, letting them describe various known
calculi (Lambda, Pi, etc.). I just want to use the descriptive power
of the bigraphs to discriminate *location* from *communication*.

