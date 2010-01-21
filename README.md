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

Odd bugs
--------

None at the moment.



