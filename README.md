stm-demo
========

Code to accompany the Boston Haskell Meetup talk on STM

Slides are here: http://www.webglog.net/slides/stm-demo-slides.html

To install:
```
git clone https://github.com/ImAlsoGreg/stm-demo.git
cd stm-demo
cabal sandbox init
cabal install
.cabal-sandbox/bin/demo m 2 2 1 +RTS -N4  (for example)
```
