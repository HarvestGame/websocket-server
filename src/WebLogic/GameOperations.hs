module WebLogic.GameOperations where
import Logic

getInitialGameState :: Num -> GameState
getInitialGameState _ = GameState {
  _units= Map.fromList[(UnitId 1, Unit{_UnitState=Idle})],
  _tick=1,
  _players=Map.fromList[(PlayerId 1, (PlayerState{_gold=1,_wood=1}))]
}

updateTick :: GameState -> GameState
updateTick gs = do
  gs._tick = gs._tick+1
  return gs
