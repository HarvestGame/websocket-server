module WebLogic.TypesSerializer (
  serializeGameState
, dserializeGameState
) where
import Logic.Types
import Data.Binary
import qualified Data.ByteString.Lazy as ByteString
import qualified Codec.Compression.GZip as GZip

instance Binary UnitId where
  put (UnitId a) = put a
  get = do a <- get; return (UnitId a)

instance Binary PlayerId where
  put (PlayerId a) = put a
  get = do a <- get; return (PlayerId a)

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

instance Binary Unit where
  put (Unit us) = put us
  get = do us <- get; return Unit{_UnitState=us}


instance Binary Building where
  put (Building hp maxhp) = do put hp; put maxhp;
  get = do hp <- get; maxhp <- get; return Building{_buildingHp=hp, _buildingMaxHp=maxhp}

instance Binary PlayerState where
  put (PlayerState gold wood) = do put gold; put wood;
  get = do gold <- get; wood <- get; return PlayerState{_gold=gold, _wood=wood}


instance Binary GameState where
  put (GameState units tick players) = do put units; put tick; put players;
  get = do units <- get; tick <- get; players <- get; return GameState{_units=units, _tick=tick, _players=players}

serializeGameState :: GameState -> ByteString.ByteString
serializeGameState gs = encode gs -- GZip.compress $ encode gs

dserializeGameState ::  ByteString.ByteString -> GameState
dserializeGameState bs = (gs :: GameState)
                      where gs = decode bs -- where gs = decode $ GZip.decompress bs


{-- main = do
  let c = GameState {
    _units= Map.fromList[(UnitId 1, Unit{_UnitState=Idle})],
    _tick=1,
    _players=Map.fromList[(PlayerId 1, (PlayerState{_gold=1,_wood=1}))]}
  let e = GZip.compress $ encode c
  print e
  let d = decode $ GZip.decompress e
  print (d :: GameState)  --}
