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
   - Add support for multiple files
   - (thomaschrstnsn) Implement a creator for torrent files
   - (axman) Turn the logging system into a better framework, add log levels,
     add process names so we can see who is doing what.
   - If we get a wrong URI, the code currently deadlocks since the tracker
     dies. Handle this problem gracefully.
   - (axman) Cleanup the BCode module, while keeping the interface somewhat
     stable. The code is an utter mess as it stands right now.
   - (axman) Improve the cabal file for the project, check with GHC 6.12.1,
     provide correct versions of needed packages.
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
   - Rewrite the tracker code to use the new monad transformer stack.
   - Improve on the command line parser. We will certainly need full-fledged
     CL parsing at some point.
   - Make the client ignore log messages beneath a certain log level
   - Make the client be able to select what processes that are allowed to
     log what (perhaps write a DSL for it).
   - When closing, gracefully tell the tracker about it.
   - When running the endgame, shuffle the returned blocks so different
     peers are likely to download different blocks.
   - Let Piece Sets be S.Set PieceNum rather than [PieceNum]. They are
     larger than 1000 for some large torrents, so it makes sense to shift to
     a better representation.

Known Bugs
----------

None known at the moment.

Before releasing into the "wild"
--------------------------------

   - The client needs to handle multi-file torrents. It is not as hard as
     it may sound - the only part of the system that needs to know about
     files is the code handling the file system. All other parts can just
     keep on transferring pieces.
   - We currently take space proportional to torrent size due to our SHA1
     calculation being slow and not use a file descriptor. Research into a
     faster SHA1 library would be really beneficial.
   - Handle Endgame. Endgame is nasty but necessary.
     Here is the list of what to do:
        * Enable handling of CANCEL messages from the ChokeMgr in Peers.
        * When peer completes a block, broadcast CANCEL messages through
          ChokeMgrP

Items for later (no particular order)
-------------------------------------

   - Add support for multiple torrents at once
   - The client needs to handle multi-file torrents. It is not as hard as
     it may sound — the only part of the system that needs to know about
     files is the code handling the file system. All other parts can just
     keep on transferring pieces.
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
   - Consider David Himmelstrups work in the packages bencode, torrent
     In the long run it would be beneficial. Short term, there is less need
     for the integration.

# vim: filetype=none tw=76 expandtab
