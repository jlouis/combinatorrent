The TODO list
=============

(This is a Markdown file)

The list of things that needs done. Feel free to take anything on the
list if you want, but do coordinate so we don't do multiple work on
the same thing. Feel free to add anything to the list as well. It
partially acts like a bug tracker at the moment in addition to being a
wish-list.

    * Change the sender Queue into having two queues, one for short
      messages and one for long messages.
    * hFlush on the sender queue in the sender process, intelligently.
      There is no reason to flush the queue before we are fairly sure
      we got all the small messages into it. It is dependent on the
      above part.
    * Optimize reading of blocks by moving it to the sender Process in
      The Peer Process
    * Don't connect twice to the same client id. Track the clients we
      connected to.
    * Handle error cases when checking a torrent file.
    * Do not connect to ourselves :)
    * Write an installation and compilation guide.
    * Write a small introduction to git bisect we can point people towards.
    * Add support for DHT
    * Add support for multiple files
    * Add rate limitation support, locally or globally
    * Add support for multiple torrents at once
    * Add prioritization support of multiTorrents
    * Support the FAST extension
    * Support UDP tracking extension
    * Support partial downloads (select files you want in the torrent)
    * Write an ETA estimator
    * (thomaschrstnsn) Implement a creator for torrent files
    * Implement a scraper on trackers
    * (axman) Turn the logging system into a better framework, add log levels,
      add process names so we can see who is doing what.
    * If we get a wrong URI, the code currently deadlocks since the tracker
      dies. Handle this problem gracefully.
    * (axman) Cleanup the BCode module, while keeping the interface somewhat
      stable. The code is an utter mess as it stands right now.
    * (axman) Improve the cabal file for the project, check with GHC 6.12.1,
      provide correct versions of needed packages.
    * Consider David Himmelstrups work in the packages bencode, torrent
      In the long run it would be beneficial. Short term, there is less need
      for the integration.
    * When we grab pieces from the Piece Manager, let it provide us with a
      pruned set of pieces we can ask with later. This way, we only need to
      consider pieces we already have once and we get a faster system.
    * The StatusP process is always fed static data. Feed it the correct data
      based on the current status: Are we a leecher or a seeder? And how much
      data is there left to download before we have the full file?

      (Hint: grep for canSeed and use the missingMap and pieceMap for the 'left'
       data)
    * Send keepalives every two minutes as per the spec.
    * Improve the rate calculation code. Use a running average such that the rate
      is fairly measured when we do rechoking ticks.
    * Make git.md into a markdown document
    * Implement extensions from http://www.bittorrent.org/beps/bep_0000.html
      which makes sense.
    * Implement sending of rates from the Peer Processes. Needed for the choking code.
    * For the histogram code, look at
      [Data.PSQueue](http://hackage.haskell.org/packages/archive/PSQueue/1.1/doc/html/Data-PSQueue.html). Ralf
      Hinze has a paper on that at [Hinze, R., A Simple Implementation
      Technique for Priority Search Queues, ICFP 2001, pp. 110-121](http://citeseer.ist.psu.edu/hinze01simple.html).
    * Consider letting the supervisors support monitoring of processes. Use this to reimplement parts
      of the PeerMgr code.
    * Update the Seeder status in PeerMgrP.
    * When stopping a Peer, put back the Pieces to the Piece Manager.
    * Interest propagation:
      - Convert PeerMgr to Transformer stack
      - Convert PieceMgr to Transformer stack
      - Let the PieceMgr tell PeerMgr when a piece is complete
      - Write a broadcast service
      - Broadcast Interest updates to the Peers by a separate broadcasting process. Make it safe when
        a peer dies.
    * Do not send HAVE messages if the Peer already has the Piece Number.

Before releasing into the "wild"
--------------------------------

    * The client needs to correctly tell the tracker how many bytes it
      uploaded and downloaded. This measure is needed on many private
      trackers as they require people to upload data back.
    * The client needs to be better at choosing the next eligible piece. Choosing one randomly is good enough.
    * The client needs to handle multi-file torrents. It is not as hard as
      it may sound — the only part of the system that needs to know about
      files is the code handling the file system. All other parts can just
      keep on transferring pieces.
    * For choking to work correctly, we must know how fast we are currently
      transferring to a peer. This is an interesting little problem if
      somebody feels their curiosity tickled :)
    * We currently take space proportional to torrent size due to our SHA1
      calculation being slow and not use a file descriptor. Research into a
      faster SHA1 library would be really beneficial.

Items for later
---------------

    * Write the Users Guide.
    * Design, build and improve a graphic UI.
    * Design, build and improve a protocol for communicating with the client.
    * Azureus/Vuze has a keepalive flood detector built-in. Consider if this
      is relevant for this client.
    * Process monitoring in general. Think.
    * We need to accept incoming connections. The system only connects
      outward at the moment
    * Write a fuzzing framework for bittorrent.

# vim: filetype=none tw=76 expandtab
