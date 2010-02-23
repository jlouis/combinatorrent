#!/usr/bin/env runhaskell

In principle, we could do with a lot less than autoconfUserhooks, but simpleUserHooks
is not running 'configure'.

We will need Distribution.Simple to do the heavyweight lifting, and we will need some filePath magic.

> import System.FilePath
> import System.Process
> import Distribution.Simple
> import Distribution.Simple.LocalBuildInfo
> import Distribution.PackageDescription

The main program is just to make Cabal lift it. But we will override testing.

> main = defaultMainWithHooks hooks
>   where hooks = autoconfUserHooks { runTests = runTests' }

Running tests is to call HaskellTorrent with its parameters for tests:

> runTests' :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
> runTests' _ _ _ lbi = system testprog >> return ()
>   where testprog = (buildDir lbi) </> "HaskellTorrent" </> "HaskellTorrent --tests"
