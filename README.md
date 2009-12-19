Haskell Torrent - a haskell bittorrent client.
==========

Introduction
----------

This is a Haskell bittorrent client. I am the introduction document
and I need to be written by some generous soul!

Usage
-----------------

Currently, it is all very simple: `make` followed by `./Main
file.torrent` is all that turns out to be supported at the moment.

The TODO list
-----------------

The list of things that needs done. Feel free to take anything on the
list if you want, but do coordinate so we don't do multiple work on
the same thing. Feel free to add anything to the list as well. It
partially acts like a bug tracker at the moment in addition to being a
wish-list.

    * Sprinkle the Peer code with debug messaging.
    * Optimize reading of blocks by moving it to the sender Process in
      The Peer Process
    * Make the system into a seeding client
    * Don't connect twice to the same client id. Track the clients we
      connected to.
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
    * Implement a creator for torrent files
    * Implement a scraper on trackers
    * Turn the logging system into a better framework, add log levels,
      add process names so we can see who is doing what.
    * If we get a wrong URI, the code currently deadlocks since the tracker
      dies. Handle this problem gracefully.
    * Cleanup the BCode module, while keeping the interface somewhat
      stable. The code is an utter mess as it stands right now.

