Implementing processes
======================

The Haskell Bittorrent project is evolving at a steady state these days. In the last couple of days, we have implemented most of the code relevant for carrying out choking of peers. Choking is the process by which you only communicate to a few peers at a time. Thus TCP/IP congestion can be avoided which drives up the download and upload rates. This post is not on this part however, which must wait a bit.

A fellow dane, Thomas Christensen, heeded my call and did some hlint runs over the code. Hopefully, we'll see more work from him ([github](http://github.com/thomaschrstnsn)). Alex Mason has added even more parsing stuff through Cereal to the code, fixed a number of bugs in the parser and improved its general state. His venture is described on his blog ([here](http://random.axman6.com/blog/)).

Processes
---------

I will be talking about processes in this post. When haskell-torrent started, our processes were simply "things spawned in the IO monad". The approach works, but it quickly becomes inadequate for several reasons. In Haskell and FP in general, a lot of power stems from the idea that we can write a large set of small building blocks and then compose them together to form increasingly larger and larger blocks as we go. When composing, we use a fairly small number of helpers -- readily present in the standard libraries.

When running in IO, we quickly end up with a lot of state. This can of course be passed around by "hand" and tail-calls. Unfortunately, this means we will end up using a lot of our precious coding time doing just exactly that. To optimize, we need a way to reflect away configuration and state when we don't need it, and reify the information at certain points in the program where it is necessary to know the state.

The ubiquitious tool in Haskell for this are Monads. Not a single monad like IO, but a for-the-case relevant monad built from monad transformers. A couple of days ago, I installed [XMonad](http://xmonad.org) by Spencer Janssen, Don Stewart, Jason Creighton and many more. This is a window manager -- but surprisingly, it needs to solve some problems similar to the one in haskell-torrent. By inspiration from XMonad, we define

> newtype Process a b c = Process (ReaderT a (StateT b IO) c)
>    deriving (Functor, Monad, MonadIO, MonadState b, MonadReader a, Typeable)

That is, a *Process* is a type. It contains some configuration data *a*, an internal state *b* and is in the process of evaluating to a value of type *c*. We use a Reader transformer so we can *ask* for configuration data when we need it. We do not expect this configuration data to be altered when the process runs. A State Transformer takes care of the internal state of the process. Finally, we let the underlying monad be IO. This gives us access to CML and the outside world.

We let GHC derive a large set of things automatically (using several extensions in the process). This gives an easier way to manipulate the state when the process is running.

Running a Process is easy:

> runP :: a -> b -> Process a b c -> IO (c, b)
> runP c st (Process p) = runStateT (runReaderT p c) st

which is exactly like in XMonad. Spawning a new process is also fairly easy:

> spawnP :: a -> b -> Process a b () -> IO ThreadId
> spawnP c st p = spawn proc
>   where proc = do runP c st p
>                   return ()

In general different processes will have different configurations *a*. These will usually contain the channels on which the process can communicate. We then define a type class

> class Logging a where
>   getLogger :: a -> LogChannel
>
> instance Logging LogChannel where
>   getLogger = id

of types that contain a logger channel. This means we can define a generic log function like this:

> log :: Logging a => String -> Process a b ()
> log msg = do
>     logC <- asks getLogger
>     liftIO $ logMsg logC msg

Type classes are a magnificent tool when we want to coerce a general function on top of different types. Many of our configurations in the client will instance the *Logging* class and then the log function knows how to access the logger in the Reader.

What does this buy us
---------------------

The advantage of doing this change on the code is twofold: First, the amount of parameter passing is considerably reduced. Reflection into the monad solves this problem. We are now able to compose easier. Function composition is considerably harder when there are many parameters abound. With the change, preliminary restructuring of the Peer process shows a much simpler flow. Also, there are now ample refactoring opportunities available with the change.

Second, the monad means we can use locality much more to our advantage. A common idiom is to modify the state of the process or to retrieve the current state for query. This now happens locally at the point where it is needed. Before, we might have needed to pass a parameter through several functions and then use it.

What next?
----------

There are a small number of things that needs to be addressed before we can claim that the client is a full bittorrent client:

  - The client needs to correctly handle the concept of *interest*. It must tell other clients if it is interested in the pieces they have at their disposal for transmission. I have some preliminary code for doing this.
  - The client needs to correctly tell the tracker how many bytes it uploaded and downloaded. This measure is needed on many private trackers as they require people to upload data back.
  - The client needs to be better at choosing the next eligible piece. Choosing one randomly is good enough.
  - The client needs to handle multi-file torrents. It is not as hard as it may sound -- the only part of the system that needs to know about files is the code handling the file system. All other parts can just keep on transferring pieces.
  - For choking to work correctly, we must know how fast we are currently transferring to a peer. This is an interesting little problem if somebody feels their curiosity tickled :)
  - We currently take space proportional to torrent size due to our SHA1 calculation being slow and not use a file descriptor. Research into a faster SHA1 library would be really beneficial.
  - We need to accept incoming connections. The system only connects outward at the moment.

And of course, it needs some testing in a non-lab setting. Currently it can seed and leech, but my setup is very simple: Opentracker and rtorrent on another computer.

The main github repository for haskell-torrent is [here](http://github.com/jlouis/haskell-torrent/).

