module Main where

import Control.Monad(when,unless,guard)
import qualified Data.Text as Text
import Data.Maybe(mapMaybe)

import Common.Basics
import Common.Field
import Common.Bag
import Common.Interact
import Common.RNG
import Common.CallJS(jsHandlers)
import Game

main :: IO ()
main = startApp App
  { appOptions = []
  , appColors = [ "red", "blue" ]
  , appJS = $(jsHandlers [ ''Input ])
  , appInitialState = \rng _opts ps ->
      case ps of
        [p1,p2] -> Right (newGame rng p1 p2)
        _       -> Left "This game needs exactly 2 players"
  , appStart = playerTurn
  }


--------------------------------------------------------------------------------

sync :: Interact ()
sync = update =<< getState

-- | Find the non-empty decks.
deckOptions :: Interact [Input]
deckOptions =
  do ds <- the gameDecks
     pure [ Deck n | (d,n) <- zip ds [ 0 .. ], not (null d) ]

-- | The other player should loose this many points, unless we'd go below 0.
doLoosePoints :: Int -> Interact ()
doLoosePoints n =
  do pid <- the gameOtherPlayer
     updateThe_ (player pid .> playerPoints) \p -> p - min n p


-- | Get some cards from the current deck
-- Assumes Deckid is OK, but may return fewer cards, if not enogh.
doGetCards :: DeckId -> Int -> Interact [Ancient]
doGetCards d n = updateThe (gameDecks .> listAt d) (splitAt n)

-- | The current player draws a card from the given deck
doDrawCards :: DeckId -> Int -> Interact ()
doDrawCards d n =
  do cs  <- doGetCards d n
     pid <- the gameCurrentPlayer
     updateThe_ (player pid .> playerHand) (bagUnion (bagFromList cs))

-- | Discard a card from the given deck
doDiscardFromDeck :: DeckId -> Interact ()
doDiscardFromDeck d =
  do cs <- doGetCards d 1
     updateThe_ gameDiscard (cs ++)

-- | Add a salvo to the current player and activate
doAddSalvo :: Interact ()
doAddSalvo =
  do this <- the gameCurrentPlayer
     updateThe_ (player this .> playerSalvos) (+ 1)
     doActivateSalvos

-- | Activate the salvos of the current player
doActivateSalvos :: Interact ()
doActivateSalvos =
  do this  <- the gameCurrentPlayer
     n     <- the (player this .> playerSalvos)
     doLoosePoints n

-- | Place the given number of copies of a card in the current player's vault.
doPlaceInVault :: [Ancient] -> Interact ()
doPlaceInVault cs =
  do this <- the gameCurrentPlayer
     updateThe_ (player this .> playerVault) (bagUnion (bagFromList cs))
     cacheBonus (length cs)

  where
  cacheBonus n =
    case n of
      0 -> pure ()
      1 -> cacheBonus1
      2 -> cacheBonus2
      3 -> cacheBonus3
      _ -> anyCacheBonus >> anyCacheBonus

  anyCacheBonus =
    do this <- the gameCurrentPlayer
       sync
       askInputs "Choose cache bonus"
          [ ( this :-> Text 0 ("Bonus " <> lab)
            , "Gain bonus for " <> lab <> " cards"
            , cacheBonus i
            )
          | i <- [ 1 .. 3 ]
          , let lab = Text.pack (show i)
          ]

  cacheBonus1 =
    do this <- the gameCurrentPlayer
       ds   <- deckOptions
       sync
       mb   <- chooseMaybe this "Choose a deck to draw 2 cards"
                 [ (i, "Draw 2 from this deck") | i <- ds ]
       case mb of
         Just (Deck i) -> doDrawCards i 2
         _             -> pure ()

  cacheBonus2 =
    do this <- the gameCurrentPlayer
       ds   <- deckOptions
       sync
       mb   <- chooseMaybe this "Use the CODE of which deck?"
                 [ (i, "Use this CODE") | i <- ds ]
       case mb of
         Just (Deck i) -> useCode i
         _             -> pure ()

  cacheBonus3 =
    do this <- the gameCurrentPlayer
       cards <- bagToList <$> the (player this .> playerHand)
       let cid = zip [ 0 .. ] cards
       sync
       mb   <- chooseMaybe this "Choose a card to play as a salvo"
                 [ (Hand i, "Play this card as a salvo") | (i,_) <- cid ]
       case mb of
         Just (Hand i) | c : _ <- [ c | (j,c) <- cid, i == j ] ->
           do updateThe_ (player this .> playerHand) (bagChange (-1) c)
              doAddSalvo
         _ -> pure ()



--------------------------------------------------------------------------------


playerTurn :: Interact ()
playerTurn =
  do drawCard
     acquisitionAction
     escalateAction
     reduceHand
     checkGameEnd
     nextTurn

checkGameEnd :: Interact ()
checkGameEnd =
  do this  <- the gameCurrentPlayer
     other <- the gameOtherPlayer
     lastP <- view gameLastPlayer
     p1    <- the (player this)
     p2    <- the (player other)
     let pointsOf     = getField playerPoints
         salvosOf     = getField playerSalvos
         valutFull p  = viewField playerVault bagSize p >= 15
         score p      = let v  = getField playerVault p
                            sz = bagSize v
                        in ( pointsOf p + sz + length (bagToNumList v)
                           , sz
                           )

     let end = or [ this == lastP && (pointsOf p1 <= 10 || pointsOf p2 <= 10)
                  , salvosOf p1 + salvosOf p2 >= 10
                  , valutFull p1
                  , valutFull p2
                  ]
     when end
       do let score1 = score p1
              score2 = score p2
          setThe (player this  .> playerPoints) (fst score1)
          setThe (player other .> playerPoints) (fst score2)
          setThe gameStatus
            case compare score1 score2 of
              LT -> EndWinner other
              EQ -> EndTie
              GT -> EndWinner this
          update =<< getState


nextTurn :: Interact ()
nextTurn =
  do ds <- the gameDecks
     when (any null ds)
       do disc <- the gameDiscard
          newCs <- updateThe gameRNG (shuffle (concat ds ++ disc))
          let each  = length newCs
              newDs = takes (replicate 3 each) newCs
          setThe gameDecks (init newDs)
          setThe gameDiscard (last newDs)
     this  <- the gameCurrentPlayer
     other <- the gameOtherPlayer
     setThe gameCurrentPlayer other
     setThe gameOtherPlayer this
     playerTurn

reduceHand :: Interact ()
reduceHand =
  do this <- the gameCurrentPlayer
     hs   <- the (player this .> playerHand)
     let n = bagSize hs
     when (n > 5) $
       do let cid = zip [ 0 .. ] (bagToList hs)
          sync
          askInputs "Choose a card to discard"
            [ (this :-> Hand i, "Discard this card"
              , do updateThe_ (player this .> playerHand) (bagChange (-1) c)
                   reduceHand
              )
            | (i,c) <- cid
            ]


-- | Draw any available card.
drawCard :: Interact ()
drawCard = drawCardExcept Nothing


-- | Draw a card from a deck, except the deck in the argument, if any.
drawCardExcept :: Maybe DeckId -> Interact ()
drawCardExcept notThis =
  do pid <- the gameCurrentPlayer
     is' <- deckOptions
     let is = case notThis of
                Nothing -> is'
                Just i  -> [ Deck j | Deck j <- is', i /= j ]
     sync
     mb  <- chooseMaybe pid "Choose a card to draw"
                                        [ (i, "Draw this card") | i <- is ]
     case mb of
       Just (Deck n) -> doDrawCards n 1
       _             -> pure ()


-- | Do an acquisition action.
acquisitionAction :: Interact ()
acquisitionAction =
  do pid <- the gameCurrentPlayer
     is  <- deckOptions
     unless (null is)
       do sync
          ~(Text n _) <-
             choose pid "Choose acquisition action"
               [ (Text 0 "Code", "Use the CODE of the top card of one deck")
               , (Text 1 "Draw", "Draw the top card of one deck and " <>
                                 "the other player loses 2 points")
               ]
          case n of
            0 ->
              do sync
                 mb <- chooseMaybe pid "Choose a card to use"
                          [ (i, "Use this CODE") | i <- is ]
                 case mb of
                   Just (Deck d) -> useCode d
                   _             -> pure ()

            _ -> do drawCard
                    doLoosePoints 2


-- | Use the code of the top car dof a given deck.
useCode :: DeckId -> Interact ()
useCode deckId =
  do cs <- the (gameDecks .> listAt deckId)
     case cs of
       [] -> pure ()
       ancient : _ ->
         case ancient of

           HeapTrawler ->
             do doLoosePoints 2
                doDrawCards deckId 1
                drawCardExcept (Just deckId)

           StiltLoper ->
             do this   <- the gameCurrentPlayer
                n      <- the (player this .> playerSalvos)
                doLoosePoints n
                doDrawCards deckId 1

           PhaseSaber ->
             do doLoosePoints 3
                doDiscardFromDeck deckId
                drawCard

           Bombarder ->
             do this <- the gameCurrentPlayer
                n    <- the (player this .> playerSalvos)
                when (n <= 2)
                  do _ <- doGetCards deckId 1
                     doAddSalvo

           VaultBot ->
             do doLoosePoints 1
                doDiscardFromDeck deckId

                this <- the gameCurrentPlayer
                is   <- deckOptions
                sync
                mb   <- chooseMaybe this "Choose a card to add to your vault"
                          [ (i,"Add this cars to your vault") | i <- is ]
                case mb of
                  Just (Deck i) ->
                    do cards <- doGetCards i 1
                       doPlaceInVault cards
                  _ -> pure ()


escalateAction :: Interact ()
escalateAction =
  do this <- the gameCurrentPlayer
     cs   <- bagToList <$> the (player this .> playerHand)
     let cids  = zip [ 0 .. ] cs

         selectCards selNum selected =
           case optSalvo ++ optVault ++ mapMaybe optCard cids of
             []   -> pure ()
             opts -> sync >> askInputs "Choose escalation action" opts
           where
           optVault =
             do guard (1 <= selNum && selNum <= 4 && allSame selected)
                pure ( this :-> Text 1 "Vault"
                     , "Place in vault"
                     , placeInVault selNum (snd (head selected))
                     )

           optSalvo =
             do guard (3 == selNum && allDifferent (map snd selected))
                pure ( this :-> Text 0 "Salvo"
                     , "Place a salvo"
                     , placeSalvo selected
                     )

           optCard (i,c)
             | i `elem` map fst selected = Nothing
             | otherwise =
               Just
               ( this :-> Hand i
               , "Select card"
               , selectCards (selNum + 1) ((i,c) : selected)
               )

     selectCards (0 :: Int) []

  where
  allSame xs = case map snd xs of
                 y : ys -> all (== y) ys
                 _      -> True

  allDifferent xs = case xs of
                      y : ys -> all (/= y) ys && allDifferent ys
                      []     -> True

  placeInVault n c =
    do this <- the gameCurrentPlayer
       updateThe_ (player this .> playerHand) (bagChange (- n) c)
       doPlaceInVault (replicate n c)


  -- XXX: what's the best way to "reveal" the cards?
  placeSalvo cs =
    do this <- the gameCurrentPlayer
       sync
       ~(Hand i) <-
          choose this "Select card to place, others will be discarded"
                    [ (Hand i, "Place this card") | (i,_) <- cs ]
       let disc = [ c | (j,c) <- cs, i /= j ]
       updateThe_ (player this .> playerHand)
                  (`bagDifference` bagFromList (map snd cs))
       updateThe_ gameDiscard (disc ++)
       doAddSalvo


