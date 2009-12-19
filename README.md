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

    * Get the Peer Manager linked up and running.
    * Optimize reading of blocks by moving it to the sender Process in
      The Peer Process
    * Make the system into a seeding client
    * Don't connect twice to the same client id. Track the clients we
      connected to.
    * Make it possible to Poison OMBox'es. Currently we leak due to them.
    * Handle error cases when checking a torrent file.
    * Add support for DHT
    * Add support for multiple files
    * Add rate limitation support
    * Add leeching support
    * Add support for multiple torrents at once
    * Add prioritization support of multiTorrents
    * Support the FAST extension
    * Support UDP tracking extension
    * Support partial downloads (select files you want in the torrent)
    * Write an ETA estimator
    * Implement a creator for torrent files
    * Implement a scraper on trackers
    

