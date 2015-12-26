Client/Server example

1. Get this repo
```
git clone https://github.com/HarvestGame/websocket-server.git
```

2. Get logic prototype

```
git cloe https://github.com/HarvestGame/logic-prototype.git
```

3. cd websocket-server
4. cabal sandbox init
5. cabal sandbox add-source ../logic-prototype
6. cabal install
7. cabal build
  * ./dist/build/server/server (.exe/.sh)
  * ./dist/build/client/client (.exe/.sh)
