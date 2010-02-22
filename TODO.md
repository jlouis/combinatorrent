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
   - When we grab pieces from the Piece Manager, let it provide us with a
     pruned set of pieces we can ask with later. This way, we only need to
     consider pieces we already have once and we get a faster system.

     When doing this, only prune pieces which are done and checked.
   - Send keepalives every two minutes as per the spec.
   - For the histogram code, look at
     [Data.PSQueue](http://hackage.haskell.org/packages/archive/PSQueue/1.1/doc/html/Data-PSQueue.html). Ralf
      Hinze has a paper on that at [Hinze, R., A Simple Implementation
     Technique for Priority Search Queues, ICFP 2001, pp. 110-121](http://citeseer.ist.psu.edu/hinze01simple.html).
   - Consider letting the supervisors support monitoring of processes. Use this to reimplement parts
     of the PeerMgr code.
   - Update the Seeder status in PeerMgrP.
   - Do not send HAVE messages if the Peer already has the Piece Number.
   - Improve on the command line parser. We will certainly need full-fledged
     CL parsing at some point.
   - Let Piece Sets be S.Set PieceNum rather than [PieceNum]. They are
     larger than 1000 for some large torrents, so it makes sense to shift to
     a better representation.
   - The status reporting code needs some help. It only transfers up/down
     rates once every 30 seconds. If a peer is living for less than 30
     seconds, then no upload/download will be reported for that peer. The
     fix is to contact the StatusP when a peer closes if it has something to
     transfer.
   - Make sure we actually seed when the torrent finishes.
     Rather hard to check with the current construction in the PeerMgr. The
     PeerMgr needs more work before this is possible.
   - Use an mmap() based interface for file I/O.

Planned for the next release
----------------------------

   - When closing, gracefully tell the tracker about it.
   - Reduce CPU load and memory load. Alternative representations of various
     data structures are needed.
   - Improve parallel execution. We are currently extremely synchronous.
   - Listen socket, this is needed to be fully BEP003 compliant. With it in
     place, we will be able to accept incoming connections as well as
     outgoing connections.
   - KeepAlive messages on sockets.
   - Improve the Peer Management code. Keep track of peers and process them
     over time in an intelligent manner.

Items for later (no particular order)
-------------------------------------

   - Add restart-support to the supervisors where applicable.
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
   - Write a fuzzing framework for bittorrent.

# vim: filetype=none tw=76 expandtab
