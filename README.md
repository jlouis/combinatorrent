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
the same thing.

    * Get the Peer Manager into a state where it can manage peers
    * Optimize reading of blocks by moving it to the sender Process in
      The Peer Process
    * Make the system into a seeding client
    * Fix Tracker bugs
    * Get the connector code in PeerP correct.
    * Don't connect twice to the same client id. Track the clients we
      connected to.
    * Use overwriteable mailboxes in the direction of PeerMgr -> Peer.
    * Make it possible to Poison OMBox'es. Currently we leak due to them.
