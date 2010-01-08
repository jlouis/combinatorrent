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
    * Consider if we need OMbox'es at all.
    * Make it possible to Poison OMBox'es. Currently we leak due to them.
    * Handle error cases when checking a torrent file.
    * Add support for DHT
    * Add support for multiple files
    * Add rate limitation support, locally or globally
    * Add leeching support
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
    * (jlouis) Make the client into an eligible leecher.
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
    * Improve the Peer Manager to the point where it can manage choking/unchoking
      of peers.
    * (jlouis) Improve stability by using supervisor primitives.
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


Items for later
---------------

    * Write the Users Guide.
    * Design, build and improve a graphic UI.
    * Design, build and improve a protocol for communicating with the client.
