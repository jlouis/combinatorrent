The TODO list
=============

(This is a Markdown file)

The list of things that needs done. Feel free to take anything on the
list if you want, but do coordinate so we don't do multiple work on
the same thing. Feel free to add anything to the list as well. It
partially acts like a bug tracker at the moment in addition to being a
wish-list.

   - Change the sender Queue into having two queues, one for short
     messages and one for long messages.
   - hFlush on the sender queue in the sender process, intelligently.
     There is no reason to flush the queue before we are fairly sure
     we got all the small messages into it. It is dependent on the
     above part.
   - Optimize reading of blocks by moving it to the sender Process in
     The Peer Process
   - Don't connect twice to the same client id. Track the clients we
     connected to.
   - Handle error cases when checking a torrent file.
   - Do not connect to ourselves :)
   - Write an installation and compilation guide.
   - Write a small introduction to git bisect we can point people towards.
   - (thomaschrstnsn) Implement a creator for torrent files
   - If we get a wrong URI, the code currently deadlocks since the tracker
     dies. Handle this problem gracefully.
   - (axman) Cleanup the BCode module, while keeping the interface somewhat
     stable. The code is an utter mess as it stands right now.
   - When we grab pieces from the Piece Manager, let it provide us with a
     pruned set of pieces we can ask with later. This way, we only need to
     consider pieces we already have once and we get a faster system.

     When doing this, only prune pieces which are done and checked.

   - The StatusP process is always fed static data. Feed it the correct data
     based on the current status: Are we a leecher or a seeder? And how much
     data is there left to download before we have the full file?

     (Hint: grep for canSeed and use the missingMap and pieceMap for the 'left'
      data)
   - Send keepalives every two minutes as per the spec.
   - Make git.md into a markdown document
   - For the histogram code, look at
     [Data.PSQueue](http://hackage.haskell.org/packages/archive/PSQueue/1.1/doc/html/Data-PSQueue.html). Ralf
      Hinze has a paper on that at [Hinze, R., A Simple Implementation
     Technique for Priority Search Queues, ICFP 2001, pp. 110-121](http://citeseer.ist.psu.edu/hinze01simple.html).
   - Consider letting the supervisors support monitoring of processes. Use this to reimplement parts
     of the PeerMgr code.
   - Update the Seeder status in PeerMgrP.
   - When stopping a Peer, put back the Pieces to the Piece Manager.
   - Do not send HAVE messages if the Peer already has the Piece Number.
   - Improve on the command line parser. We will certainly need full-fledged
     CL parsing at some point.
   - When closing, gracefully tell the tracker about it.
   - Let Piece Sets be S.Set PieceNum rather than [PieceNum]. They are
     larger than 1000 for some large torrents, so it makes sense to shift to
     a better representation.
   - Cleanup the code around ChokeMgrP.advancePeerChain. It currently does a
     lot of stuff it doesn't have to do.

Known Bugs
----------

None known at the moment.

Before releasing into the "wild"
--------------------------------

   - We currently take space proportional to torrent size due to our SHA1
     calculation being slow and not use a file descriptor. Research into a
     faster SHA1 library would be really beneficial.
   - Make sure we actually seed when the torrent finishes.
   - Check that the tracker is told about what happened.
   - Introduce status update messages to the tracker. When the torrent is
     started, stopped or completes, we should message the tracker with the
     update right away. The tracker code already supports this, but the rest
     of the code must do the right thing.
   - We don't update the Status Process correctly from the peers. Why?
   - What about reentrancy and FFI OpenSSL calls?
   - Handle the following bug:
            "The 'impossible' happened, are you implementing endgame?"
     in PieceMgrP. We basically want to figure out what should happen.

Items for later (no particular order)
-------------------------------------

   - Improve parallel execution. We are currently extremely synchronous.
   - Add support for multiple torrents at once
   - Add prioritization support of multiTorrents
   - Implement a scraper on trackers
   - Implement extensions from http://www.bittorrent.org/beps/bep_0000.html
     which makes sense. See the README file.
   - Support the FAST extension
   - Add rate limitation support, locally or globally
   - Add support for DHT
   - Support UDP tracking extension
   - Support partial downloads (select files you want in the torrent)
   - Write an ETA estimator
   - Write the Users Guide.
   - Design, build and improve a graphic UI.
   - Design, build and improve a protocol for communicating with the client.
   - Azureus/Vuze has a keepalive flood detector built-in. Consider if this
     is relevant for this client.
   - Process monitoring in general. Think.
   - We need to accept incoming connections. The system only connects
     outward at the moment
   - Write a fuzzing framework for bittorrent.

# vim: filetype=none tw=76 expandtab
