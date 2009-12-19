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

    * Change the sender Queue into having two queues, one for short
      messages and one for long messages.
    * hFlush on the sender queue in the sender process, intelligently.
      There is no reason to flush the queue before we are fairly sure
      we got all the small messages into it. It is dependent on the
      above part.
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


Odd bugs
--------

Here is one we need to figure out. My guess is signedness somewhere
when doing conversions. The solution is to use the right types I guess

    jlouis@illithid:~/Projects/haskell-torrent$ ./Main testfile.txt.torrent
    Created channels
    Started logger
    Created various data
    Started Status Process
    Looping PeerMgr
    Started Tracker Process
    Timer in 2 seconds
    Got Tracker Event
    Sending event to Tracker
    Main: Numeric.showIntAtBase: applied to negative number -2000450983540949075
    Main: Numeric.showIntAtBase: applied to negative number -2000450983540949075


