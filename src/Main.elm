module Main exposing (main)

import Browser
import Random
import Html exposing (button, div, text)
import Html.Events exposing (onClick)
import Html exposing (input)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onInput)
import Array exposing (Array)
import Debug exposing (log)

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = (\_ -> Sub.none)
    , view = view
    }


-- MAIN MODEL

type Session 
  = InLobby LobbyState
  | ActiveGame GameState
  | PostGame Int


type alias Model = Session

init : () -> (Model, Cmd Msg)
init _ =
  ( ActiveGame sampleGame -- InLobby emptyLobby
  , Cmd.none
  )


-- MAIN UPDATE

type Msg =
    StartGame
  | QuitGame
  | Game GameEvent
  | Lobby LobbyEvent

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (model, msg) of
    (InLobby state, StartGame) ->
      case state.players of 
        first :: players ->
          ( ActiveGame <| gameStart first players
          , Cmd.none
          )
        [] ->
          ( InLobby state
          , Cmd.none
          )
    (InLobby state, Lobby event) ->
      ( InLobby (updateLobby event state)
      , processLobbyEvent event
      )
    (ActiveGame _, QuitGame) ->
      ( PostGame 0
      , Cmd.none
      )
    (ActiveGame gameState, Game Roll) ->
      ( ActiveGame gameState
      , Random.generate (\id -> Game <| NewDice id) roll
      )
    (ActiveGame gameState, Game EndTurn) ->
      ( ActiveGame (passToNextPlayer gameState)
      , Cmd.none
      )
    (ActiveGame gameState, Game (NewDice dice)) ->
      ( ActiveGame ({ gameState | currentPlayer = givePlayerDice dice gameState.currentPlayer })
      , Cmd.none
      )
    (ActiveGame gameState, Game (Choose die)) ->
      let 
        _ = if isDieChoiceLegal die gameState then
              log "die choice is playable" ()
            else
              log "die choice is not playable" ()
      in
        ( ActiveGame gameState
        , Cmd.none
        )
    _ ->
      ( model
      , Cmd.none
      )

roll : Random.Generator (List Die) 
roll =
  Random.list 4 (Random.int 1 6)

pairs : List ((Int, Int), (Int, Int))
pairs = [ ((0, 1), (2, 3))
        , ((0, 2), (1, 3))
        , ((0, 3), (1, 2))
        ]

indexSum : (Int, Int) -> Array Int -> Int
indexSum (i, j) arr =
  case (Array.get i arr, Array.get j arr) of
    (Just a, Just b) ->
      a + b
    _ ->
      0

sumPairs : Array Int -> List (Int, Int)
sumPairs arr =
  List.map (\(pair1, pair2) -> (indexSum pair1 arr, indexSum pair2 arr)) pairs

-------------------- MAIN VIEW --------------------

renderLadderProgess : Array Int -> String
renderLadderProgess ladderPositions = 
  Array.toList ladderPositions
  |> List.map String.fromInt
  |> List.intersperse ", "
  |> String.concat

renderDice : List Int -> Html.Html Msg
renderDice dice = 
  let
    -- 0x267f is the unicode point right before the die faces
    dieVisualization dieValue = (0x267f + dieValue) |> Char.fromCode |> String.fromChar
    dicePairSums = sumPairs (Array.fromList dice)
    pairVisualization (sum1, sum2) =
      button [ onClick (Game (Choose sum1))] [ text (String.concat [String.fromInt sum1, " and ", String.fromInt sum2]) ]
  in
    div [] [ div [ Html.Attributes.style "font-size" "225% "] (List.map (\dieValue ->
                                  text (dieVisualization dieValue)) 
                                  dice)
           , div [] (List.map pairVisualization dicePairSums)
           ]

renderPhaseText : ActivePlayer -> Html.Html Msg
renderPhaseText player =
  case player.turn of
      Betting ->
        text "player is betting"
      Selecting dice ->
        div [] [ (text "player is choosing from " )
               , renderDice dice
               ]

renderPlacedTracker tracker = String.concat
  [ "Tracker climbing ladder "
  , String.fromInt tracker.ladder
  , " on rung "
  , String.fromInt tracker.position
  ]

renderTrackers : ActivePlayer -> Html.Html Msg
renderTrackers player =
  let
    placedTrackers = List.map (\t -> text (renderPlacedTracker t)) player.trackers
    unplacedTrackers = if player.tokens > 0 then
                          (text ("player has " ++ (String.fromInt player.tokens) ++ " unplaced trackers"))
                       else
                          text "player has no other trackers"
  in
    div [] (unplacedTrackers :: placedTrackers)

renderPlayer : Player -> Html.Html Msg
renderPlayer player = 
  Html.li [] [text (player.name ++ "'s ladder positions are [" ++ (renderLadderProgess player.progress) ++ "]" )]

renderActivePlayer : ActivePlayer -> Html.Html Msg
renderActivePlayer activePlayer =
  div [] [ renderPlayer activePlayer.player
         , renderPhaseText activePlayer
         , renderTrackers activePlayer
         ]

view : Session -> Html.Html Msg
view model =
  div [] (case model of
            InLobby lobbyState -> 
              [ button [ onClick StartGame ] [ text "Start a new game" ]
              , button [ onClick (Lobby AddPlayer) ] [ text "Add new player" ]
              , viewLobby lobbyState
              ]
            ActiveGame gameState ->
              [ Html.ul [] (Html.li [] [renderActivePlayer gameState.currentPlayer] :: (List.map renderPlayer gameState.waitingPlayers))
              , button [ onClick (Game Roll) ] [ text "Roll dice" ]
              , button [ onClick (Game EndTurn) ] [ text "End turn" ]
              , button [ onClick QuitGame ] [ text "Quit this game" ]
              ]
            PostGame score ->
              [ text ("Final Score: " ++ (String.fromInt score))
              , button [ onClick StartGame ] [ text "Start another game" ]
              ]
          )

renderPotentialPlayer : Player -> Html.Html Msg
renderPotentialPlayer player =
  Html.li [] [
    input [ Html.Attributes.id (String.fromInt player.id), type_ "text", placeholder "enter player's name", value player.name, onInput (\text -> Lobby (NamePlayer (player.id, text)))] []
  ]

-------------------- LOBBY MODEL --------------------

type alias LobbyState =
  { players : List Player 
  }

emptyLobby : LobbyState
emptyLobby = { players = [] }

-------------------- LOBBY UPDATE --------------------

type LobbyEvent =
    AddPlayer 
  | NewPlayer Int
  | NamePlayer (Int, String)
  | RemovePlayer Int

updateLobby : LobbyEvent -> LobbyState -> LobbyState
updateLobby event state =
    case event of
      NewPlayer id ->
        ({ state | players = (
          { id = id
          , name = ""
          , progress = Array.repeat 13 0  -- 13 ladders all starting at position 0 (aka not on the ladder)
          } :: state.players)
        })
      NamePlayer (id, name) ->
        ({ state | players = 
          ( List.map
            (\player ->
                if player.id == id then
                  {player | name = name}
                else player
            )
            state.players
          )
        })
      RemovePlayer id ->
        ({ state | players =
          List.filter (\p -> p.id /= id) state.players
         })
      AddPlayer -> state

generatePlayerId : Random.Generator Int
generatePlayerId = Random.int 0 (2^31)

processLobbyEvent : LobbyEvent -> Cmd Msg
processLobbyEvent event =
  case event of
    AddPlayer ->
      Random.generate (\id -> Lobby (NewPlayer id)) generatePlayerId
    _ ->
      Cmd.none

-------------------- LOBBY VIEW --------------------

viewLobby : LobbyState -> Html.Html Msg
viewLobby lobby =
  Html.ol [] (List.map renderPotentialPlayer lobby.players)

-------------------- GAME MODEL --------------------

type alias Die = Int

type alias Player =
  { id : Int
  , name : String
  , progress : Array Int -- state of ladder position for all 13 ladders, 0 means not on the ladder
  } 

type alias Tracker =
  { ladder: Int       -- the index of the ladder it's on
  , position: Int     -- the rung the tracker is on (0 means it's not placed)
  }

availableTracker : Tracker
availableTracker =
  { ladder = 0
  , position = 0
  }

type alias ActivePlayer =
  { player: Player
  , turn: TurnPhase
  , trackers: List Tracker  -- placed trackers
  , tokens: Int             -- unplaced trackers
  }

selectPlayer : Player -> ActivePlayer
selectPlayer player =
  { player = player
  , turn = Betting
  , trackers = []
  , tokens = 3
  }

givePlayerDice : List Die -> ActivePlayer -> ActivePlayer
givePlayerDice dice player =
  { player | turn = Selecting dice}

dieAppliesToTracker : Die -> Tracker -> Bool
dieAppliesToTracker die tracker =
  die == tracker.ladder

isDieChoiceLegal : Die -> GameState -> Bool
isDieChoiceLegal die gameState =
  let
    playerCanUseDie = List.any (dieAppliesToTracker die) gameState.currentPlayer.trackers
    playerHasTokens = gameState.currentPlayer.tokens > 0
    laddersClosed = 
      List.foldl
        heighestProgress
        (Array.repeat 13 0 |> Array.toList)
        (reducePlayers (\p -> Array.toList p.progress) gameState)
      |> completedLadders
      |> Array.fromList
  in
    case (Array.get (die - 1) laddersClosed) of
        Just False ->
          playerCanUseDie || playerHasTokens
        _ ->
          False



--advanceTrackerIfPossible : Die -> ActivePlayer -> Result String ActivePlayer 
--advanceTrackerIfPossible die activePlayer =
--  let
--    (playableTrackers, otherTrackers) =
--      List.partition (dieAppliesToTracker die) activePlayer.trackers
--  in
--    case playableTrackers of
--      tracker :: ts ->
--        (nextPositionForTracker tracker) :: (ts ++ otherTrackers)
--      [] ->
--
--
--nextPositionForTracker : Tracker -> Maybe Tracker
--nextPositionForTracker tracker =
--  let
--    ladderHeight = 13 - 2 * abs (5 - tracker.position)
--  in
--    if tracker.position == ladderHeight then
--      None
--    else
--      Just { tracker | position = tracker.position + 1 }
--
--pushTrackerIfPossible : Die -> Tracker -> Result String Tracker
--pushTrackerIfPossible die tracker =
--  if die == tracker.ladder then
--    Ok (nextPositionForTracker tracker)
--  else if tracker.ladder == 0 then
--    Ok { ladder = die, position = 1 }
--  else
--    Err "die does not apply to this tracker"
    
--  

--temporaryScoreChoice : ActivePlayer -> Int -> ActivePlayer
--temporaryScoreChoice activePlayer die =
--  { activePlayer | player = }

type TurnPhase =
    Betting
  | Selecting (List Die)   -- TODO: will change to set of 3 pairs...

type alias GameState = 
  { currentPlayer: ActivePlayer
  , waitingPlayers: List Player
  }

gameStart : Player -> List Player -> GameState
gameStart firstPlayer otherPlayers =
    { currentPlayer = selectPlayer firstPlayer
    , waitingPlayers = otherPlayers
    }

sampleGame : GameState
sampleGame =
    { currentPlayer = selectPlayer { id = 0, name = "Alice", progress = Array.repeat 13 0 }
    , waitingPlayers =
        [ { id = 1, name = "Bob", progress = Array.fromList [3, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0] }
        , { id = 2, name = "Charlie", progress = Array.fromList [0, 3, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0] }
        ]
    }

--- This could be generated with [13 - 2 * abs(5 - x) | x <- 0..11]
--- but hardcoding it is so much easier to see and understand...
ladderHeights : Array Int
ladderHeights = Array.fromList [3, 5, 7, 9, 11, 13, 11, 9, 7, 5, 3]

completedLadders : List Int -> List Bool
completedLadders ladderProgress =
  List.map2 (>=) ladderProgress (Array.toList ladderHeights)

heighestProgress : List Int -> List Int -> List Int
heighestProgress progress1 progress2 =
  List.map2 max progress1 progress2

--joinCompletedLadders : Array Bool -> Array Bool -> Array Bool
--joinCompletedLadders ladders1 ladders2 =
--  List.map2 (||) ladders1 ladders2

-- GAME UPDATE

type GameEvent =
    Roll
  | NewDice (List Int)
  | Choose Int
  | EndTurn


-- GAME HELPER METHODS

passToNextPlayer : GameState -> GameState
passToNextPlayer gameState =
    case gameState.waitingPlayers of 
        nextPlayer :: remainingPlayers ->
            { currentPlayer = selectPlayer nextPlayer
            , waitingPlayers = remainingPlayers ++ [gameState.currentPlayer.player]   -- TODO: transfer tracker state to progress state
            }
        [] ->
            gameState
    --List.Nonempty.append (List.Nonempty.pop players) (List.Nonempty.singleton <| List.Nonempty.head players)

reducePlayers : (Player -> a) -> GameState -> List a
reducePlayers reducer gameState =
    (reducer gameState.currentPlayer.player) :: List.map reducer gameState.waitingPlayers


      
