import Data.Binary
import qualified Data.Map as Map
type Id = Int

newtype PlayerId = PlayerId Id deriving (Show)
newtype UnitId = UnitId Id deriving (Show)

instance Binary UnitId where
  put (UnitId a) = put a
  get = do a <- get; return (UnitId a)

instance Binary PlayerId where
  put (PlayerId a) = put a
  get = do a <- get; return (PlayerId a)

type Kind = Int
newtype BuildingKind = BuildingKind Kind

data UnitState = Idle | Moving | Harvesting | Fighting deriving (Show)

instance Binary UnitState where
   put Idle = put "Idle"
   put Moving = put "Moving"
   put Harvesting = put "Harvesting"
   put Fighting = put "Fighting"
   get = do
      tag_ <- get
      case tag_ of
         "Idle" -> return Idle
         "Moving" -> return Moving
         "Harvesting" -> return Harvesting
         "Fighting" -> return Fighting

data Unit = Unit {
    _UnitState :: UnitState
    } deriving (Show)


instance Binary Unit where
  put (Unit us) = put us
  get = do us <- get; return Unit{_UnitState=us}


data Building = Building {
    _buildingHp :: Int,
    _buildingMaxHp :: Int
    }

instance Binary Building where
  put (Building hp maxhp) = do put hp; put maxhp;
  get = do hp <- get; maxhp <- get; return Building{_buildingHp=hp, _buildingMaxHp=maxhp}


data PlayerState = PlayerState {
    _gold :: Int,
    _wood :: Int
    } deriving (Show)

instance Binary PlayerState where
  put (PlayerState gold wood) = do put gold; put wood;
  get = do gold <- get; wood <- get; return PlayerState{_gold=gold, _wood=wood}

data GameState = GameState {
    _units :: Map.Map UnitId Unit,
    _tick :: Int, -- | this is the tick number of the current game
    _players :: Map.Map PlayerId PlayerState
    } deriving (Show)

instance Binary GameState where
  put (GameState units tick players) = do put units; put tick; put players;
  get = do units <- get; tick <- get; players <- get; return GameState{_units=units, _tick=tick, _players=players}

main = do
  let c = GameState {
    _units= Map.empty,
    _tick=1,
    _players=Map.empty}
  let e = encode c
  print e
  let d = decode e
  print (d :: GameState)
