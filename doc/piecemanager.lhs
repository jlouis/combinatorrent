On the idioms used in haskell-torrent
=====================================

This post serves several purposes. First, we bring an update on the
code state: The client is now able to leech, that is download, in a
lab setting. The lab consist of a test tracker, an rtorrent client two
computers. The tesfile is file of 5000000 bytes containing a specific
phrase over and over. This gives good compression ratio in git and
thus it is not that irritating to move over the wire if needed.

Alex Mason did a good job improving several things in the project. He
added prioritization to logging, got compilation working on GHC 6.12.x
and is now looking at improving every aspect of data parsing. The
BCode work, able to handle the bittorrent bencoded data using
applicative functors (**Data.Applicative**) look really good.

The latter part serves two purposes in parallel: It describes the used
idioms and it describes the piece manager code used for leeching
torrents. This is a work in progress and there are some things we
don't handle yet.

Also, I am trying to use John MacFarlane’s excellent
[pandoc](http://johnmacfarlane.net/pandoc/) package for typesetting
this in blogger.

The Piecemanager
----------------

The Piece Manager is defined in the module **PieceMgrP**. It is
responsible for keeping track of the torrents internal Piece
State. That is, what do we need to download, and what have we already
downloaded and can serve to other peers. Remember that in our
terminology, a Block is a subset of a piece, given by an offset and a
length. You could call it a *slice* of a piece.

The basic idiom of the piecemanager is that of an owning process. In
traditional semaphore-oriented programming we protect a data structure
by a mutex. We could also protect it software transactions, but since
we partially derived haskell-torrent from etorrent, we'll go with the
message passing method. We simple invent a process to manage the data
structure. Operations on the structure is then serialized by passing
them to the process and gettings answers back, RPC-style. It might not
be parallel, but it certainly is easy.

The data structure we want to protect is the piece database:

> data PieceDB = PieceDB
>    { pendingPieces :: [PieceNum]
>    , donePiece     :: [PieceNum]
>    , inProgress    :: M.Map PieceNum InProgressPiece
>    , infoMap       :: PieceMap
>    }

The database contains a list of pieces which are pending for
download. This list should, in a perfect world, be maintained with a
histogram such that we know the rarity of each piece. A *good* client
prefers rare pieces to young pieces and selects randomly among pieces
of the same rarity. A weaker client picks randomly without taking
their rarity into consideration. A dumb client, like haskell-torrent
currently, just picks them off from a single end. This is bad for the
torrent cloud to do, but it is a start. If someone comes up with a
data structure which is (practically) efficient for storing
histograms, I would be happy to hear about it.

The *donePiece* record field is the list of pieces that are done. We
keep this around because when a new peer is connected to the client
then we need to tell this peer about what pieces we have fully
downloaded.

Then we have **Data.Map** **Map** which tells us something about
Pieces that are in progress. The InProgress data type has the
following structure:

> data InProgressPiece = InProgressPiece
>    { ipDone  :: Int
>    , ipSize  :: Int
>    , ipHaveBlocks :: S.Set Block
>    , ipPendingBlocks :: [Block]
>    } deriving Show

These fields are (in order) how many blocks in the piece we have
downloaded, the complete size of the piece, a set of the blocks we
have downloaded and a list of blocks pending download. The size of the
piece is almost always the same, but the last piece is different if
the complete file is not a multiple of the block size.

Returning to the *PieceDB*, the last entry describes the complete
torrent. The PieceMap tells, for each piece, its offset in the file,
its length and its SHA1 digest. Note we do not support multi-file
torrents yet, although this code would probably be unaltered. The
offset is in the multi-file-case the offset in the concatenation of
the files in the torrent.

## Starting the process

The PieceManager process is started with the *start* call:

> start :: LogChannel -> PieceMgrChannel -> FSPChannel -> PieceDB -> IO ()
> start logC mgrC fspC db = (spawn $ lp db) >> return ()
>   where lp db = do
>           msg <- sync $ receive mgrC (const True)
>           case msg of

We supply a number of **CML**-channels to the process from the outside
and then we spawn off the main loop before returning (). This is
probably not good in the long run, where we will need to handle errors
in the process. But for now we accept that the code is supposedly
error-free and never have any problems.

The loop code itself synchronizes on messages here named *msg*. These
messages have the form

> data PieceMgrMsg = GrabBlocks Int [PieceNum] (Channel [(PieceNum, [Block])])
>                  | StoreBlock PieceNum Block B.ByteString
>                  | PutbackBlocks [(PieceNum, Block)]
>                  | GetDone (Channel [PieceNum])

and each of these are going to be explained in the following. A
general note is that if the message contains a channel, it is usually
a form of RPC-message, where the channel is the return channel on
which to send back an answer. One could probably factor this out into
a generic RPC-construction with a bit of cleverness, but I have not
given it much thought yet.

In the following, we present some examples of processing these messages.

### Grabbing blocks

Here is the fragment for grabbing blocks. It is one of the paths in
the diagram above.

> GrabBlocks n eligible c ->
>    do logMsg logC $ "Grabbing blocks"
>       let (blocks, db') = grabBlocks' n eligible db
>       logMsg logC $ "Grabbed..."
>       sync $ transmit c blocks
>       lp db'

Basically, this call is a request by a peer process to get exclusive
access to *n* blocks among all the available blocks for a while. This
ensures that only this peer will download the blocks, eliminating
block collisions. The *eligible* value is a list of pieces that the
peer has already downloaded, and we should of course only choose
blocks among those. Our block grabber may honor the request or return
an empty list if no block can be satisfied.

The guts is hidden in the call to the helper *grabBlocks'*, which we
will describe later.

### Storing blocks

The other path in the diagram is maintained by this code fragment:

>  StoreBlock pn blk d ->
>      do FSP.storeBlock fspC pn blk d
>         let (done, db') = updateProgress db pn blk
>         if done
>            then do assertPieceComplete db pn logC
>                    pieceOk <- FSP.checkPiece fspC pn
>                    let db'' =
>                      case pieceOk of
>                        Nothing ->
>                          error "PieceMgrP: Piece Nonexisting!"
>                        Just True -> completePiece db' pn
>                        Just False -> putBackPiece db' pn
>                    lp db''
>             else lp db'

We get a request to store the block *blk* in piece *pn* where *d* is
the data we want to store as a **ByteString**. We invoke the
filesystem to actually store the block. In a real world, we would
check that this is really what we wanted. If the piece is already
complete and checked, we don't want a stray block to errornously
destroy the piece. In general we want more checks like these in the
client.

Then we update the database with the progress on the piece. If the
piece is done, we invoke a checking of that piece. Either it is
complete and Ok, in which case we mark it as such in the database --
or it is not Ok, in which case we put it back for downloading. This
happens in the real world at times due to clients with bugs so it is
always important not to trust the other client. If the piece
completes, we should send out HAVE messages to all connected peers. I
plan to make the Peer Manager do that, but the code has not yet been
implemented for that.

### The RPC idiom

When we initially connect to a peer, we will have to transfer the
pieces we have. To do this, we construct a BITFIELD message and to
construct this, we need the set of pieces which are complete. The
*GetDone* message handles this:

> GetDone c -> do sync $ transmit c (donePiece db)
>                 lp db

and the peer which want this calls the function

> getPieceDone :: PieceMgrChannel -> IO [PieceNum]
> getPieceDone ch = do
>  c <- channel
>  sync $ transmit ch $ GetDone c
>  sync $ receive c (const True)

I think there is an idiom to be extracted from this code and as it is
used quite a lot it would be very beneficial to have in the long run.

## Grabbing blocks, the inner workings

The diagram above hints that grabbing blocks has a more complicated
control flow. The idea of the code is that the control flow is modeled
by a tail call into the next box. Let us break up the code:

> grabBlocks' :: Int -> [PieceNum] -> PieceDB
>             -> ([(PieceNum, [Block])], PieceDB)
> grabBlocks' k eligible db = tryGrabProgress k eligible db []
>   where

So we will try to grab *k* blocks from the *eligible* pieces from
*db*. The empty list is the accumulator in which we add the blocks we
found as we go.

>    tryGrabProgress 0 _  db captured = (captured, db) -- Enough, exit
>    tryGrabProgress n ps db captured =
>        case ps `intersect` (fmap fst $ M.toList (inProgress db)) of
>          []  -> tryGrabPending n ps db captured
>          (h:_) -> grabFromProgress n ps h db captured

Grabbing from the pieces already in progress minimizes the number of
"open" pieces. We want to minimize this as a complete piece can be
checked for correctness. Correct pieces can then be shared to others
and since our download speed is dependent on our upload speed,
complete pieces is key. Finding a piece is simply to carry out set
intersection. If you've read to this point, there is a refactoring
opportunity by using *M.keys*.

A smarter client, as hinted, would order the eligible pieces such that
the rarest pieces were tried first. One could also toy with the idea
of *pruning*: tell the peer what pieces we already have downloaded so
it won't include them in further requests. This would keep the *ps*
parameter small in size.

There are three exit-points. Either there was a piece, *h*, we can
grab from that one, or there were none among the pieces in progress:
we will then seek the list of pending pieces. Finally, if we are
requested to grab 0 blocks, we already have enough blocks and can
return those we have together with the new database.

>   grabFromProgress n ps p db captured =
>       let ipp = fromJust $ M.lookup p (inProgress db)
>           (grabbed, rest) = splitAt n (ipPendingBlocks ipp)
>           nIpp = ipp { ipPendingBlocks = rest }
>           nDb  = db { inProgress = M.insert p nIpp (inProgress db) }
>       in
>           if grabbed == []
>            then tryGrabProgress n (ps \\ [p]) db captured
>            else tryGrabProgress (n - length grabbed) ps nDb ((p, grabbed) : captured)

Here, we have found the piece *p* as being in progress. So we find its
*InProgress* structure, and use **Data.List.splitAt** to cut off as
many blocks as possible. We may find that we can't grab any
blocks. This happens when we or other peers are already downloading
all pieces. We then prune the piece *p* from the set of eligible
pieces and try again. Otherwise, we update the database and the number
of pieces to grab and go back to start. Another hint: We should
probably prune *p* from *ps* here as well :)

>   tryGrabPending n ps db captured =
>       case ps `intersect` (pendingPieces db) of
>         []    -> (captured, db) -- No (more) pieces to download, return
>         (h:_) ->
>             let blockList = createBlock h db
>                 ipp = InProgressPiece 0 bSz S.empty blockList
>                 bSz = len $ fromJust $ M.lookup n (infoMap db)
>                 nDb = db { pendingPieces = (pendingPieces db) \\ [h],
>                            inProgress    = M.insert h ipp (inProgress db) }
>             in tryGrabProgress n ps nDb captured

Finally, this part grabs from the pending pieces. Either we can't find
a piece and then we can just exit. In the other case we have found a
piece. We then create the blocks of the piece and insert it into the
pieces in progress. The tail-call to *tryGrabProgress* will then find
it.

I hope the similarity to the diagram is clear. When building these
tail-call mazes I usually start with the diagram sketch. Then I
hand-write them in Graphviz's dot-notation and create them. Finally I
write the code.