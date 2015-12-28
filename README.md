Client/Server example

1. Get this repo

    git clone https://github.com/HarvestGame/websocket-server.git

2. Get logic prototype

    git clone https://github.com/HarvestGame/logic-prototype.git

3. `cd websocket-server`
4. `cabal sandbox init`
5. `cabal sandbox add-source ../logic-prototype`
6. `cabal install`
7. `cabal build`
  * `cabal run server`
  * `cabal run client`
