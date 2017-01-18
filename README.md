Combinatorrent - a bittorrent client.
=====================================

[![Build Status](https://secure.travis-ci.org/jlouis/combinatorrent.svg?branch=master)](http://travis-ci.org/jlouis/combinatorrent)

Introduction
----------

This is a bittorrent client. I am the introduction document and I need to be
written by some generous soul!

Installation
------------

Here is what I do to install haskell torrrent locally on my machine:

    cabal install --prefix=$HOME --user

Since we are using the magnificient cabal, this is enough to install haskell
torrent in our $HOME/bin directory.

Usage
-----------------

Combinatorrent can currently only do one very simple thing. If you call it with

    Combinatorrent foo.torrent

then it will begin downloading the file in foo.torrent to the current
directory via the Bittorrent protocol.

Protocol support
----------------

Currently haskell-torrent supports the following BEPs (See the
[BEP Process](http://www.bittorrent.org/beps/bep_0000.html) document for an
explanation of these)

   - 0003, 0004, 0006, 0010, 0020,

Combinatorrent implicitly supports these extensions

   - 0027: Support by the virtue of only supporting a single tracker and no
     DHT.

Partially supported extensions:

   - 0007: Combinatorrent understands and uses the "peers6" response from
     the tracker to connect clients. On the other hand, it does nothing to
     provide the "ipv4=" and "ipv6=" keys on tracker requests. As such, it
     can be claimed that 0007 support is available, as everything we left
     out is only qualified as MAY.

   - 0023: Combinatorrent supports the "compact" response only, although it
     is explicitly stated that the client must support both. In practice it
     has little impact as all modern trackers will only return compact
     responses anyway.

Combinatorrent is not supporting these BEPs, but strives to do so one day:

   - 0005, 0009, 0012, 0015, 0016, 0017, 0018, 0019, 0021, 0022,
     0024, 0026, 0028, 0029, 0030, 0031, 0032

Debugging
---------

For debugging, jlouis tends to use the following:

    make conf build test

This builds Combinatorrent with the *Debug* flag set and also builds the
software with profiling by default so it is easy to hunt down performance
regressions. It also runs the internal test-suite for various values. There
are a couple of interesting targets in the top-level Makefile

Reading material for hacking Combinatorrent:
--------------------------------------------

   - [Protocol specification - BEP0003](http://www.bittorrent.org/beps/bep_0003.html):
     This is the original protocol specification, tracked into the BEP
     process. It is worth reading because it explains the general overview
     and the precision with which the original protocol was written down.

   - [Bittorrent Enhancement Process - BEP0000](http://www.bittorrent.org/beps/bep_0000.html)
     The BEP process is an official process for adding extensions on top of
     the BitTorrent protocol. It allows implementors to mix and match the
     extensions making sense for their client and it allows people to
     discuss extensions publicly in a forum. It also provisions for the
     deprecation of certain features in the long run as they prove to be of
     less value.

   - [wiki.theory.org](http://wiki.theory.org/Main_Page)
     An alternative description of the protocol. This description is in
     general much more detailed than the BEP structure. It is worth a read
     because it acts somewhat as a historic remark and a side channel. Note
     that there are some commentary on these pages which can be disputed
     quite a lot.

   - ["Supervisor Behaviour"](http://erlang.org/doc/design_principles/sup_princ.html)
     From the Erlang documentation. How the Erlang Supervisor behaviour
     works. The Supervisor and process structure of Combinatorrent is
     somewhat inspired by the Erlang ditto.

Source code Hierarchy
---------------------

   - **Data**: Data structures.
      - **Queue**: Functional queues. Standard variant with two lists.
      - **PendingSet**: A wrapper around Data.PSQueue for tracking how
        common a piece is.
      - **PieceSet**: BitArrays of pieces and their operations.

   - **Process**: Process definitions for the different processes comprising
                  Combinatorrent
      - **ChokeMgr**: Manages choking and unchoking of peers, based upon the current speed of the peer
        and its current state. Global for multiple torrents.
      - **Console**: Simple console process. Only responds to 'quit' at the moment.
      - **DirWatcher**: Watches a directory and adds any torrent present in
        it.
      - **FS**: Process managing the file system.
      - **Listen**: Not used at the moment. Step towards listening sockets.
      - **Peer**: Several process definitions for handling peers. Two for sending, one for receiving
        and one for controlling the peer and handle the state.
      - **PeerMgr**: Management of a set of peers for a single torrent.
      - **PieceMgr**: Keeps track of what pieces have been downloaded and what are missing. Also hands
        out blocks for downloading to the peers.
      - **Status**: Keeps track of uploaded/downloaded/left bytes for a single torrent. Could be globalized.
      - **Timer**: Timer events.
      - **TorrentManager**: Manages torrents at the top-level.
      - **Tracker**: Communication with the tracker.

   - **Protocol**: Modules for interacting with the various bittorrent protocols.
      - **BCode**: The bittorrent BCode coding. Used by several protocols.
      - **Wire**: The protocol used for communication between peers.

   - **Top Level**:
      - **Channels**: Various Channel definitions.
      - **Combinatorrent**: Main entry point to the code. Sets up processes.
      - **Digest**: SHA1 digests as used in the bittorrent protocol.
      - **FS**: Low level Filesystem code. Interacts with files.
      - **Process**: Code for Erlang-inspired processes.
      - **RateCalc**: Rate calculations for a network socket. We use this to keep track of the
        current speed of a peer in one direction.
      - **Supervisor**: Erlang-inspired Supervisor processes.
      - **Test.hs**: Code for test-framework
      - **TestInstance.hs**: Various helper instances not present in the test framework by default
      - **Torrent**: Various helpers and types for Torrents.
      - **Tracer**: Code for simple "ring"-like tracing.
      - **Version.hs.in**: Generates **Version.hs** via the configure script.

<!-- vim: filetype=none tw=76 expandtab
-->
