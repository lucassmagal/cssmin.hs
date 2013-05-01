cssmin.hs (based on cssmin.py)
==============================

This script is a simple attempt to learn how to program more
complex programs in Haskell (instead of, you know, "fat n = n * fat (n-1)").
It's highly based on Zachary Voase's [cssmin] script in Python.

To execute it, you need "regex-compat":

    $ sudo cabal update
    $ sudo cabal install regex-compat

    # To execute:
    $ runhaskell cssmin.hs normalize.css > normalize.min.css

    # Or you can compile
    $ ghc cssmin.hs -o cssmin
    $ ./cssmin normalize.css > normalize.min.css

[cssmin]: https://github.com/zacharyvoase/cssmin
