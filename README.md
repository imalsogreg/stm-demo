stm-demo
========

Code to accompany the Boston Haskell Meetup talk on STM

To install:
```
git clone https://github.com/ImAlsoGreg/stm-demo.git
cd stm-demo
cabal sandbox init
cabal install
.cabal-sandbox/bin/demo m 2 2 1 +RTS -N4  (for example)
```
