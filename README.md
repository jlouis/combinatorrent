Haskell Torrent - a haskell bittorrent client.
==========

Introduction
----------

This is a Haskell bittorrent client. I am the introduction document
and I need to be written by some generous soul!

Installation
------------

Here is what I do to install haskell torrrent locally on my machine:

    cabal install --prefix=$HOME --user

Since we are using the magnificient cabal, this is enough to install haskell torrent in our $HOME/bin directory.

Usage
-----------------

Haskell torrent can currently only do one very simple thing. If you call it with

    HaskellTorrent foo.torrent

then it will begin downloading the file in foo.torrent to the current directory via the Bittorrent protocol. *Note:* Currently we have no support for multifile torrents.

Protocol support
----------------

Currently haskell-torrent supports the following BEPs (See the
[BEP Process](http://www.bittorrent.org/beps/bep_0000.html) document for an
explanation of these)

   - 004, 020,

Haskell-torrent is not supporting these BEPs, but strives to do so one day:

   - 003, 005, 006, 007, 010, 012, 015, 009, 023, 018, 021, 022, 024, 026, 027,
     028, 029, 030, 031, 032

Haskell-torrent will probably never support these BEPs:

   - 016, 017, 019

Debugging
---------

For debugging, jlouis tends to use the following:

    make conf build test

This builds HaskellTorrent with the *Debug* flag set and also builds the software with profiling
by default so it is easy to hunt down performance regressions. It also runs the internal test-suite
for various values.

Source code Hierarchy
---------------------

   - **Data**: Data structures.
      - **Queue**: Functional queues. Standard variant with two lists.

   - **Process**: Process definitions for the different processes comprising Haskell Torrent
      - **ChokeMgr**: Manages choking and unchoking of peers, based upon the current speed of the peer
        and its current state. Global for multiple torrents.
      - **Console**: Simple console process. Only responds to 'quit' at the moment.
      - **FS**: Process managing the file system.
      - **Listen**: Not used at the moment. Step towards listening sockets.
      - **Peer**: Several process definitions for handling peers. Two for sending, one for receiving
        and one for controlling the peer and handle the state.
      - **PeerMgr**: Management of a set of peers for a single torrent.
      - **PieceMgr**: Keeps track of what pieces have been downloaded and what are missing. Also hands
        out blocks for downloading to the peers.
      - **Status**: Keeps track of uploaded/downloaded/left bytes for a single torrent. Could be globalized.
      - **Timer**: Timer events.
      - **Tracker**: Communication with the tracker.

   - **Protocol**: Modules for interacting with the various bittorrent protocols.
      - **BCode**: The bittorrent BCode coding. Used by several protocols.
      - **Wire**: The protocol used for communication between peers.

   - **Top Level**:
      - **Digest**: SHA1 digests as used in the bittorrent protocol.
      - **FS**: Low level Filesystem code. Interacts with files.
      - **HaskellTorrent**: Main entry point to the code. Sets up processes.
      - **Logging**: Logging interface.
      - **LoggingTypes**: Types and instances used by the Logging framework.
      - **PeerTypes**: Types used by peers.
      - **Process**: Code for Erlang-inspired processes.
      - **RateCalc**: Rate calculations for a network socket. We use this to keep track of the
        current speed of a peer in one direction.
      - **Supervisor**: Erlang-inspired Supervisor processes.
      - **Torrent**: Various helpers and types for Torrents.
      - **Version.hs.in**: Generates **Version.hs** via the configure script.
      - **Test.hs**: Code for test-framework
      - **TestInstance.hs**: Various helper instances not present in the test framework by default

Known bugs
----------

    "PieceMgrP"(Fatal):	Process exiting due to ex: user error (P/Blk (655,Block {blockOffset = 81920, blockSize = 16384}) is in the HaveBlocks set)
    "ConsoleP"(Info):	Process Terminated by Supervisor


