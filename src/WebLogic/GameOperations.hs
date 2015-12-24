module WebLogic.GameOperations (
  getInitialGameState
, updateTick
) where
import Logic.Types
import qualified Data.Map as Map

getInitialGameState :: Int -> GameState
getInitialGameState _ = GameState {
  _units= Map.fromList[(UnitId 1, Unit{_UnitState=Idle})],
  _tick=1,
  _players=Map.fromList[(PlayerId 1, (PlayerState{_gold=1,_wood=1}))]
}

updateTick :: GameState -> GameState
updateTick (GameState {_units=u, _tick=t, _players=p}) = GameState{_units=u, _tick=t+1, _players=p}
