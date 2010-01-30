#!/usr/bin/env runhaskell

In principle, we could do with a lot less than autoconfUserhooks, but simpleUserHooks
is not running 'configure'.

>import Distribution.Simple
>main = defaultMainWithHooks autoconfUserHooks
