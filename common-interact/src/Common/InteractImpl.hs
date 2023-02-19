module Common.InteractImpl
  ( -- * InteractionState
    InteractState
  , startGame
  , GameInfo(..)
  , handleMessage
  , PlayerRequest
  , OutMsg

  -- * Building Interactions
  , Interact
  , askInputs
  , choose
  , chooseMaybe
  , view
  , update
  , localUpdate
  , localUpdate_
  , getState

  , the
  , setThe
  , updateThe
  , updateThe_

  , save
  ) where

import Data.Text(Text)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.List(foldl')
import Control.Monad(liftM,ap)
import GHC.Generics(Generic)

import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:?))
import qualified Data.Aeson as JS

import Common.Basics
import Common.Field
import AppTypes(State,StateView,UpdateView,Input,Update,doUpdate,finalState,
                                  playerView,playerUpdateView)




startGame :: GameInfo -> [WithPlayer Input] -> InteractState
startGame ginfo moves = foldl' step state0 moves
  where
  state0 = interaction_ (Left (gInit ginfo))
           InteractState { iInit    = ginfo
                         , iLog     = []
                         , iName    = 0
                         , iGame    = gState ginfo
                         , iAsk     = Map.empty
                         , iQuestion = ""
                         , iShouldSave = False
                         }
  step state i = interaction_ (Right i) state

data GameInfo = GameInfo
  { gPlayers :: Set PlayerId
  , gState   :: State
  , gInit    :: Interact ()
  , gSave    :: [WithPlayer Input] -> String
  }


data OutMsg =
    CurGameState CurState
  | SetQuestion Text
  | AskQuestions (Text, [ChoiceHelp])
  | GameUpdate UpdateView
    deriving Generic

data ChoiceHelp = ChoiceHelp
  { chChoice    :: Input
  , chHelp      :: Text
  , chStateName :: Int    -- ties question to state
  } deriving Generic

data PlayerRequest =
    Reload
  | Undo
  | PlayerResponse ChoiceHelp


handleMessage ::
  WithPlayer PlayerRequest ->
  InteractState -> (InteractState, [WithPlayer OutMsg], Maybe (Int,String))
handleMessage (p :-> req) =
  case req of
    Reload -> \s -> (s, [reload s p], Nothing)

    Undo -> \s ->
      case iLog s of
        (q :-> _) : more | p == q ->
           let s1 = startGame (iInit s) (reverse more)
           in (s1, map (reload s1) (iPlayers s1), Nothing)
        _ -> (s,[],Nothing)

    PlayerResponse ch ->
      \s -> if chStateName ch == iName s
              then askQuestions (interaction (Right (p :-> chChoice ch)) s)
              else (s,[],Nothing)


  where
  askQuestions (s,os) =
    ( s { iShouldSave = False }
    , reverse os ++
      [ q :-> SetQuestion (iQuestion s) | q <- iPlayers s ] ++
      [ q :-> AskQuestions (iQuestion s, qs)
      | (q,qs) <- Map.toList
                $ Map.fromListWith (flip (++))
                  [ (q,[ChoiceHelp { chChoice = ch
                                   , chHelp = help
                                   , chStateName = iName s
                                   }])
                      | (q :-> ch,(help,_))  <- Map.toList (iAsk s) ]
      ]
    , if iShouldSave s
         then Just (length (iLog s), gSave (iInit s) (reverse (iLog s)))
         else Nothing
    )



reload :: InteractState -> PlayerId -> WithPlayer OutMsg
reload s p =
  p :-> CurGameState
        CurState { cGame = playerView p (iGame s)
                 , cQuestions =
                      ( iQuestion s
                      , [ ChoiceHelp { chChoice = q
                                     , chHelp = help
                                     , chStateName = iName s
                                     }
                        | (p' :-> q,(help,_)) <- Map.toList (iAsk s)
                        , p' == p ]
                      )
                 }



data InteractState =
  InteractState
    { iInit   :: GameInfo
      -- ^ Information about how the game was initialized.

    , iLog    :: [WithPlayer Input]
      -- ^ A record of all responses made by the players

    , iName   :: !Int
      -- ^ An identifier for the state.  Used to check that the anser match
      -- the question.

    , iGame   :: State
      -- ^ The current game state.
      -- Should be reproducable by replyaing the log file on the initial state

    , iAsk     :: Map (WithPlayer Input) (Text, R)
      -- ^ Choices avialable to the players.

    , iQuestion :: Text
      -- ^ A desicription of what we are doing.

    , iShouldSave :: Bool
      -- ^ Indicates if this is a save point.
    }

iPlayers :: InteractState -> [PlayerId]
iPlayers = Set.toList . gPlayers . iInit


-- | A monad for writing interactive turn based games.
newtype Interact a = Interact ((a -> R) -> R)

instance Functor Interact where
  fmap = liftM

instance Applicative Interact where
  pure a = Interact \k -> k a
  (<*>)  = ap

instance Monad Interact where
  Interact m >>= f = Interact \k -> m \a -> let Interact m1 = f a
                                            in m1 k

type R = InteractState -> [Update] -> (InteractState, [Update])

-- | Perform an interaction
interaction ::
  Either (Interact ()) (WithPlayer Input) ->
  InteractState -> (InteractState, [WithPlayer OutMsg])
interaction how s = (s1,msgs)
  where
  Interact m =
     case how of
       Left m'    -> m'
       Right resp -> continueWith resp

  (s1,os) = m (\_ -> (,)) s []
  msgs    = [ p :-> o | p <- iPlayers s1
                      , o <- map (GameUpdate . playerUpdateView p) os ]

interaction_ :: Either (Interact ()) (WithPlayer Input) ->
               InteractState -> InteractState
interaction_ how = fst . interaction how

-- | Choose one of the given options.
-- The list of possible answers should be non-empty.
choose ::
  PlayerId       {- ^ Ask this player -} ->
  Text           {- ^ Description of what we are asking the player -} ->
  [(Input,Text)] {- ^ List of possible answers, with description -} ->
  Interact Input {- ^ Chosen answer -}
choose playerId q opts =
  askInputs q [ (playerId :-> ch, help, pure ch) | (ch,help) <- opts ]

-- | Ask a question but only if there is a choice.
-- Returns `Nothing` if there are no valid answers.
-- If there is only a single valid answer,
-- then select it without asking the player.
chooseMaybe ::
  PlayerId               {- ^ Ask this player -} ->
  Text                   {- ^ Description of what we are asking the player -} ->
  [(Input,Text)]         {- ^ List of possible answers, with description -} ->
  Interact (Maybe Input) {- ^ Chosen answer, if any -}
chooseMaybe playerId q opts =
  case opts of
    []  -> pure Nothing
    [t] -> pure (Just (fst t))
    _   -> Just <$> choose playerId q opts

-- | Ask one or more players a question and continue based on their answer.
askInputs ::
  Text {- ^ Desciption of what we are asking -} ->
  [ (WithPlayer Input, Text, Interact a) ]
  {- ^ Possible answers for various players.

       * The first component specifies the player and what they see.
       * The second is a description of the choice.
       * The third is how to continue if this answer is selected. -} ->
  Interact a
askInputs q opts =
  Interact $
  \curK ->
  \curS os ->
  let cont (ch,help,Interact m) = (ch, (help, m curK))
  in (curS { iQuestion = q
           , iAsk = Map.union (Map.fromList (map cont opts)) (iAsk curS) }, os)



-- | Resume execution based on player input
continueWith :: WithPlayer Input -> Interact a
continueWith msg = Interact $
  \_ ->
  \s os ->
    case Map.lookup msg (iAsk s) of
      Just (_,f)  ->
        let s1 = s { iAsk = Map.empty
                   , iLog = msg : iLog s, iName = iName s + 1 }
        in f s1 os
      Nothing -> (s,os)

-- | Access a component of the current game state
view :: (State -> a) -> Interact a
view f = Interact $
  \k ->
  \s os -> case iGame s of
             st | finalState st -> (s,os)
                | otherwise -> k (f st) s os

-- | Get the value of the given field of the state
the :: Field State a -> Interact a
the f = view (getField f)

-- | Access the current game state
getState :: Interact State
getState = view id

-- | Update the current game state
update :: Update -> Interact ()
update o = Interact $
  \k    ->
  \s os -> case iGame s of
             a | finalState a -> (s,os)
               | otherwise    -> k () s { iGame = doUpdate o a } (o : os)

-- | Updates that are not visible to the players
localUpdate :: (State -> (a,State)) -> Interact a
localUpdate f = Interact $
  \k ->
  \s os -> case iGame s of
             a | finalState a -> (s,os)
               | otherwise    -> case f a of
                                   (x,b) -> k x s { iGame = b } os

-- | Updates that are not visible to the players
localUpdate_ :: (State -> State) -> Interact ()
localUpdate_ f = localUpdate \s -> ((),f s)

setThe :: Field State a -> a -> Interact ()
setThe x a = localUpdate_ (setField x a)

updateThe :: Field State a -> (a -> (b,a)) -> Interact b
updateThe x f = localUpdate (updField' x f)

updateThe_ :: Field State a -> (a -> a) -> Interact ()
updateThe_ x f = localUpdate_ (updField x f)


-- | Save the current game state.
save :: Interact ()
save = Interact $ \k s os -> k () s { iShouldSave = True } os


--------------------------------------------------------------------------------
-- Input and output messages

data CurState = CurState
  { cGame       :: StateView
  , cQuestions  :: (Text, [ChoiceHelp])
  }


instance ToJSON OutMsg

instance ToJSON CurState where
  toJSON g = JS.object
    [ "game"      .= cGame g
    , "questions" .= cQuestions g
    ]

instance FromJSON PlayerRequest where
  parseJSON =
    JS.withObject "request" \o ->
    do tag <- o .:? "tag"
       case tag :: Maybe Text of
         Just "reload" -> pure Reload
         Just "undo"   -> pure Undo
         _ -> PlayerResponse <$> parseJSON (JS.Object o)


instance ToJSON ChoiceHelp
instance FromJSON ChoiceHelp

--------------------------------------------------------------------------------

