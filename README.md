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

Source code Hierarchy
---------------------

   - **Data**: Data structures.
      - **Queue**: Functional queues. Standard variant with two lists.

   - **Process**: Process definitions for the different processes comprising Haskell Torrent
      - **Timer**: Timer events.

   - **Protocol**: Modules for interacting with the various bittorrent protocols.
      - **BCode**: The bittorrent BCode coding. Used by several protocols.
      - **Wire**: The protocol used for communication between peers.

   - **Top Level**:
      - **HaskellTorrent**: Main entry point to the code. Sets up processes.

Odd bugs
--------

None at the moment.



